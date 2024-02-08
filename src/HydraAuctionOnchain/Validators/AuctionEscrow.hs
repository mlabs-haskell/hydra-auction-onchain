{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.AuctionEscrow
  ( PAuctionEscrowRedeemer
      ( StartBiddingRedeemer
      , BidderBuysRedeemer
      , SellerReclaimsRedeemer
      , CleanupAuctionRedeemer
      )
  , auctionEscrowValidator
  , pisConcluding
  ) where

import HydraAuctionOnchain.Errors.Validators.AuctionEscrow (PAuctionEscrowError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueInputWithScriptHash
  , pfindUniqueOutputWithAddress
  , pfindUniqueOutputWithScriptHash
  , pgetOwnInput
  , ponlyOneInputFromAddress
  , putxoAddress
  , pvaluePaidToAddr
  , pvaluePaidToScript
  )
import HydraAuctionOnchain.Lib.Address (paddrPaymentKeyHash)
import HydraAuctionOnchain.Types.AuctionEscrowState
  ( PAuctionEscrowState (AuctionConcluded)
  , pvalidateAuctionEscrowTransitionToAuctionConcluded
  , pvalidateAuctionEscrowTransitionToBiddingStarted
  )
import HydraAuctionOnchain.Types.AuctionTerms
  ( PAuctionTerms
  , pbiddingPeriod
  , pcleanupPeriod
  , ppenaltyPeriod
  , ppurchasePeriod
  , ptotalAuctionFees
  )
import HydraAuctionOnchain.Types.BidTerms (psellerPayout, pvalidateBidTerms)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe, passertMaybeData)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState (PStandingBidState))
import HydraAuctionOnchain.Types.Tokens
  ( pauctionTokenBundleBurned
  , ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  )
import Plutarch.Api.V1.Value (plovelaceValueOf, pnormalize)
import Plutarch.Api.V2
  ( PAddress
  , PCurrencySymbol
  , PPubKeyHash
  , PScriptContext
  , PScriptHash
  , PTxInInfo
  , PTxInfo
  )
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.Maybe (pdnothing)
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------
-- Redeemers
--------------------------------------------------------------------------------

data PAuctionEscrowRedeemer (s :: S)
  = StartBiddingRedeemer (Term s (PDataRecord '[]))
  | BidderBuysRedeemer (Term s (PDataRecord '[]))
  | SellerReclaimsRedeemer (Term s (PDataRecord '[]))
  | CleanupAuctionRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PAuctionEscrowRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PAuctionEscrowRedeemer)

pisConcluding :: Term s (PAuctionEscrowRedeemer :--> PBool)
pisConcluding = phoistAcyclic $
  plam $ \redeemer -> pmatch redeemer $ \case
    BidderBuysRedeemer _ -> pcon PTrue
    SellerReclaimsRedeemer _ -> pcon PTrue
    _ -> pcon PFalse

--------------------------------------------------------------------------------
-- Validator
--------------------------------------------------------------------------------

auctionEscrowValidator
  :: Term
      s
      ( PScriptHash
          :--> PScriptHash
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PAuctionEscrowRedeemer
          :--> PScriptContext
          :--> PUnit
      )
auctionEscrowValidator = phoistAcyclic $
  plam $ \standingBidSh feeEscrowSh auctionCs auctionTerms oldAuctionState redeemer ctx -> P.do
    txInfo <- plet $ pfield @"txInfo" # ctx
    sellerAddress <- plet $ pfield @"sellerAddress" # auctionTerms

    -- The seller address should contain payment key hash:
    sellerPkh <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'Error'InvalidSellerAddress)
          (paddrPaymentKeyHash # sellerAddress)

    -- The validator's own input should exist.
    ownInput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'Error'MissingAuctionEscrowInput)
          (pgetOwnInput # ctx)

    -- There should only be one auction escrow input.
    ownAddress <- plet $ putxoAddress # ownInput
    passert $(errCode AuctionEscrow'Error'TooManyOwnScriptInputs) $
      ponlyOneInputFromAddress # ownAddress # txInfo

    -- The auction escrow input should contain an auction
    -- escrow token.
    passert $(errCode AuctionEscrow'Error'OwnInputMissingToken) $
      ptxOutContainsAuctionEscrowToken # auctionCs #$ pfield @"resolved" # ownInput

    -- Branching checks based on the redeemer used.
    pmatch redeemer $ \case
      StartBiddingRedeemer _ ->
        pcheckStartBidding
          # standingBidSh
          # txInfo
          # auctionCs
          # auctionTerms
          # oldAuctionState
          # ownAddress
          # sellerPkh
      BidderBuysRedeemer _ ->
        pcheckBidderBuys
          # standingBidSh
          # feeEscrowSh
          # txInfo
          # auctionCs
          # auctionTerms
          # oldAuctionState
          # ownAddress
      SellerReclaimsRedeemer _ ->
        pcheckSellerReclaims
          # feeEscrowSh
          # txInfo
          # auctionCs
          # auctionTerms
          # oldAuctionState
          # ownAddress
          # sellerPkh
      CleanupAuctionRedeemer _ ->
        pcheckCleanupAuction
          # txInfo
          # auctionCs
          # auctionTerms
          # oldAuctionState
          # ownInput
          # sellerPkh

--------------------------------------------------------------------------------
-- StartBidding
--------------------------------------------------------------------------------

pcheckStartBidding
  :: Term
      s
      ( PScriptHash
          :--> PTxInfo
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PAddress
          :--> PPubKeyHash
          :--> PUnit
      )
pcheckStartBidding = phoistAcyclic $
  plam $ \standingBidSh txInfo auctionCs auctionTerms oldAuctionState ownAddress sellerPkh -> P.do
    txInfoFields <- pletFields @["mint", "signatories", "validRange"] txInfo

    -- There should be no tokens minted or burned.
    passert $(errCode AuctionEscrow'StartBidding'Error'UnexpectedTokensMintedBurned) $
      pfromData txInfoFields.mint #== mempty

    -- This redeemer can only be used during
    -- the bidding period.
    passert $(errCode AuctionEscrow'StartBidding'Error'IncorrectValidityInterval) $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoFields.validRange

    -- The transaction should be signed by the seller.
    passert $(errCode AuctionEscrow'StartBidding'Error'MissingSellerSignature) $
      ptxSignedBy
        # txInfoFields.signatories
        # pdata sellerPkh

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- The auction escrow output should contain an auction
    -- escrow token.
    passert $(errCode AuctionEscrow'StartBidding'Error'AuctionEscrowOutputMissingToken) $
      ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput

    -- The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- The auction state should transition from
    -- `AuctionAnnounced` to `BiddingStarted`.
    passert $(errCode AuctionEscrow'StartBidding'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToBiddingStarted
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check standing bid state
    ----------------------------------------------------------------------------

    -- There should be exactly one standing bid output.
    standingBidOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'MissingStandingBidOutput)
          (pfindUniqueOutputWithScriptHash # standingBidSh # txInfo)

    -- The standing bid output's datum should be decodable
    -- as a standing bid state.
    initialBidState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum # standingBidOutput)

    -- The standing bid state should be initialized
    -- without bid terms.
    passert $(errCode AuctionEscrow'StartBidding'Error'InvalidStandingBidState) $
      initialBidState #== pcon (PStandingBidState pdnothing)

    pcon PUnit

--------------------------------------------------------------------------------
-- StartBidding
--------------------------------------------------------------------------------

pcheckBidderBuys
  :: Term
      s
      ( PScriptHash
          :--> PScriptHash
          :--> PTxInfo
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PAddress
          :--> PUnit
      )
pcheckBidderBuys = phoistAcyclic $
  plam $ \standingBidSh feeEscrowSh txInfo auctionCs auctionTerms oldAuctionState ownAddress -> P.do
    txInfoFields <- pletFields @["mint", "signatories", "validRange"] txInfo
    auctionTermsFields <- pletFields @["auctionLot", "sellerAddress"] auctionTerms

    -- There should be no tokens minted or burned.
    passert $(errCode AuctionEscrow'BidderBuys'Error'UnexpectedTokensMintedBurned) $
      pfromData txInfoFields.mint #== mempty

    -- This redeemer can only be used during
    -- the purchase period.
    passert $(errCode AuctionEscrow'BidderBuys'Error'IncorrectValidityInterval) $
      pcontains # (ppurchasePeriod # auctionTerms) # txInfoFields.validRange

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- The auction escrow output should contain
    -- the auction escrow token.
    passert
      $(errCode AuctionEscrow'BidderBuys'Error'AuctionEscrowOutputMissingAuctionEscrowToken)
      (ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput)

    -- The auction escrow output should contain
    -- the standing bid token.
    passert
      $(errCode AuctionEscrow'BidderBuys'Error'AuctionEscrowOutputMissingStandingBidToken)
      (ptxOutContainsStandingBidToken # auctionCs # ownOutput)

    -- The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- The auction state should transition from
    -- `BiddingStarted` to `AuctionConcluded`.
    passert $(errCode AuctionEscrow'BidderBuys'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToAuctionConcluded
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check auction lot transfer to the winning bidder
    ----------------------------------------------------------------------------

    -- There should be exactly one standing bid input.
    standingBidInput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'MissingStandingBidInput)
          (pfindUniqueInputWithScriptHash # standingBidSh # txInfo)

    -- The standing bid input should contain the standing
    -- bid token.
    standingBidInputResolved <- plet $ pfield @"resolved" # standingBidInput
    passert $(errCode AuctionEscrow'BidderBuys'Error'StandingBidInputMissingToken) $
      (ptxOutContainsStandingBidToken # auctionCs # standingBidInputResolved)

    -- The standing bid output's datum should be decodable
    -- as a standing bid state.
    bidState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum @PStandingBidState # standingBidInputResolved)

    -- The standing bid should contain bid terms.
    bidTerms <-
      plet $
        passertMaybeData
          $(errCode AuctionEscrow'BidderBuys'Error'EmptyStandingBid)
          (pto bidState)

    -- The bidder address should contain payment key hash.
    bidderAddress <- plet $ pfield @"biBidderAddress" #$ pfield @"btBidder" # bidTerms
    bidderPkh <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'InvalidBidderAddress)
          (paddrPaymentKeyHash # bidderAddress)

    -- The bid terms in the standing bid input are valid.
    passert $(errCode AuctionEscrow'BidderBuys'Error'BidTermsInvalid) $
      pvalidateBidTerms # auctionCs # auctionTerms # bidTerms

    -- The auction lot is paid to the winning bidder,
    -- who is buying it.
    passert $(errCode AuctionEscrow'BidderBuys'Error'AuctionLotNotPaidToBidder) $
      auctionTermsFields.auctionLot #<= pvaluePaidToAddr # txInfo # bidderAddress

    -- The bidder signed the transaction.
    passert $(errCode AuctionEscrow'BidderBuys'Error'NoBidderConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata bidderPkh

    ----------------------------------------------------------------------------
    -- Check ADA payment to the seller
    ----------------------------------------------------------------------------

    -- The seller receives the proceeds of the auction.
    passert $(errCode AuctionEscrow'BidderBuys'Error'SellerPaymentIncorrect) $
      (psellerPayout # auctionTerms # bidTerms)
        #<= (plovelaceValueOf #$ pvaluePaidToAddr # txInfo # auctionTermsFields.sellerAddress)

    ----------------------------------------------------------------------------
    -- Check auction fees
    ----------------------------------------------------------------------------

    -- The total auction fees are sent to
    -- the fee escrow validator.
    passert $(errCode AuctionEscrow'BidderBuys'Error'PaymentToFeeEscrowIncorrect) $
      (ptotalAuctionFees # auctionTerms)
        #<= (plovelaceValueOf #$ pvaluePaidToScript # txInfo # feeEscrowSh)

    pcon PUnit

--------------------------------------------------------------------------------
-- SellerReclaims
--------------------------------------------------------------------------------

pcheckSellerReclaims
  :: Term
      s
      ( PScriptHash
          :--> PTxInfo
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PAddress
          :--> PPubKeyHash
          :--> PUnit
      )
pcheckSellerReclaims = phoistAcyclic $
  plam $ \feeEscrowSh txInfo auctionCs auctionTerms oldAuctionState ownAddress sellerPkh -> P.do
    txInfoFields <- pletFields @["mint", "signatories", "validRange"] txInfo
    auctionTermsFields <- pletFields @["auctionLot", "sellerAddress"] auctionTerms
    let sellerAddress = auctionTermsFields.sellerAddress

    -- There should be no tokens minted or burned.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'UnexpectedTokensMintedBurned) $
      pfromData txInfoFields.mint #== mempty

    -- This redeemer can only be used during
    -- the penalty period.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'IncorrectValidityInterval) $
      pcontains # (ppenaltyPeriod # auctionTerms) # txInfoFields.validRange

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'SellerReclaims'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- The auction escrow output should contain
    -- the auction escrow token.
    passert
      $(errCode AuctionEscrow'SellerReclaims'Error'AuctionEscrowOutputMissingAuctionEscrowToken)
      (ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput)

    -- The auction escrow output should contain
    -- the standing bid token.
    passert
      $(errCode AuctionEscrow'SellerReclaims'Error'AuctionEscrowOutputMissingStandingBidToken)
      (ptxOutContainsStandingBidToken # auctionCs # ownOutput)

    -- The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'SellerReclaims'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- The auction state should transition from
    -- `BiddingStarted` to `AuctionConcluded`.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToAuctionConcluded
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check auction lot transfer back to the seller
    ----------------------------------------------------------------------------

    -- The auction lot is returned to the seller.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'PaymentToSellerIncorrect) $
      auctionTermsFields.auctionLot
        #<= (pvaluePaidToAddr # txInfo # sellerAddress)

    -- The seller signed the transaction.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'NoSellerConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata sellerPkh

    ----------------------------------------------------------------------------
    -- Check auction fees
    ----------------------------------------------------------------------------

    -- The total auction fees are sent to
    -- the fee escrow validator.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'PaymentToFeeEscrowIncorrect) $
      (ptotalAuctionFees # auctionTerms)
        #<= (plovelaceValueOf #$ pvaluePaidToScript # txInfo # feeEscrowSh)

    pcon PUnit

--------------------------------------------------------------------------------
-- CleanupAuction
--------------------------------------------------------------------------------

pcheckCleanupAuction
  :: Term
      s
      ( PTxInfo
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PTxInInfo
          :--> PPubKeyHash
          :--> PUnit
      )
pcheckCleanupAuction = phoistAcyclic $
  plam $ \txInfo auctionCs auctionTerms auctionState ownInput sellerPkh -> P.do
    txInfoFields <- pletFields @["mint", "signatories", "validRange"] txInfo

    -- The auction state, auction metadata,and standing bid
    -- tokens of the auction should all be burned. No other tokens
    -- should be minted or burned.
    passert $(errCode AuctionEscrow'CleanupAuction'Error'AuctionTokensNotBurnedExactly) $
      pnormalize # txInfoFields.mint #== pauctionTokenBundleBurned # auctionCs

    -- This redeemer can only be used during the cleanup period.
    passert $(errCode AuctionEscrow'CleanupAuction'Error'IncorrectValidityInterval) $
      pcontains # (pcleanupPeriod # auctionTerms) # txInfoFields.validRange

    -- The seller signed the transaction.
    passert $(errCode AuctionEscrow'CleanupAuction'Error'NoSellerConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata sellerPkh

    -- The auction is concluded.
    passert $(errCode AuctionEscrow'CleanupAuction'Error'AuctionIsNotConcluded) $
      auctionState #== pcon (AuctionConcluded pdnil)

    -- The auction escrow input contains the standing bid
    -- token in addition to the auction token.
    passert $(errCode AuctionEscrow'CleanupAuction'Error'AuctionEscrowInputMissingStandingBidToken) $
      ptxOutContainsStandingBidToken # auctionCs #$ pfield @"resolved" # ownInput

    pcon PUnit
