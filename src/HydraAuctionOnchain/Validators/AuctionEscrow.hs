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

import HydraAuctionOnchain.Errors.AuctionEscrow (PAuctionEscrowError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueInputWithScriptHash
  , pfindUniqueOutputWithAddress
  , pfindUniqueOutputWithScriptHash
  , pgetOwnInput
  , ponlyOneInputFromAddress
  , ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  , putxoAddress
  , pvaluePaidTo
  , pvaluePaidToScript
  )
import HydraAuctionOnchain.Types.AuctionEscrowState
  ( PAuctionEscrowState
  , pvalidateAuctionEscrowTransitionToAuctionConcluded
  , pvalidateAuctionEscrowTransitionToBiddingStarted
  )
import HydraAuctionOnchain.Types.AuctionTerms
  ( PAuctionTerms
  , pbiddingPeriod
  , ppenaltyPeriod
  , ppurchasePeriod
  , ptotalAuctionFees
  )
import HydraAuctionOnchain.Types.BidTerms (psellerPayout, pvalidateBidTerms)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe, passertMaybeData)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState (PStandingBidState))
import Plutarch.Api.V1.Value (plovelaceValueOf)
import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo)
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

    -- (AUES0) The validator's own input should exist.
    ownInput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'Error'MissingAuctionEscrowInput)
          (pgetOwnInput # ctx)

    -- (AUES1) There should only be one auction escrow input.
    ownAddress <- plet $ putxoAddress # ownInput
    passert $(errCode AuctionEscrow'Error'TooManyOwnScriptInputs) $
      ponlyOneInputFromAddress # ownAddress # txInfo

    -- (AUES2) The auction escrow input should contain an auction
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
      CleanupAuctionRedeemer _ ->
        undefined

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
          :--> PUnit
      )
pcheckStartBidding = phoistAcyclic $
  plam $ \standingBidSh txInfo auctionCs auctionTerms oldAuctionState ownAddress -> P.do
    txInfoFields <- pletFields @["mint", "signatories", "validRange"] txInfo

    -- (AUES3) There should be no tokens minted or burned.
    passert $(errCode AuctionEscrow'StartBidding'Error'UnexpectedTokensMintedBurned) $
      pfromData txInfoFields.mint #== mempty

    -- (AUES4) This redeemer can only be used during
    -- the bidding period.
    passert $(errCode AuctionEscrow'StartBidding'Error'IncorrectValidityInterval) $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoFields.validRange

    -- (AUES5) The transaction should be signed by the seller.
    passert $(errCode AuctionEscrow'StartBidding'Error'MissingSellerSignature) $
      ptxSignedBy
        # txInfoFields.signatories
        # (pfield @"sellerPkh" # auctionTerms)

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- (AUES6) There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- (AUES7) The auction escrow output should contain an auction
    -- escrow token.
    passert $(errCode AuctionEscrow'StartBidding'Error'AuctionEscrowOutputMissingToken) $
      ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput

    -- (AUES8) The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- (AUES9) The auction state should transition from
    -- `AuctionAnnounced` to `BiddingStarted`.
    passert $(errCode AuctionEscrow'StartBidding'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToBiddingStarted
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check standing bid state
    ----------------------------------------------------------------------------

    -- (AUES10) There should be exactly one standing bid output.
    standingBidOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'MissingStandingBidOutput)
          (pfindUniqueOutputWithScriptHash # standingBidSh # txInfo)

    -- (AUES11) The standing bid output's datum should be decodable
    -- as a standing bid state.
    initialBidState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum # standingBidOutput)

    -- (AUES12) The standing bid state should be initialized
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
    auctionTermsFields <- pletFields @["auctionLot", "sellerPkh"] auctionTerms

    -- (AUES13) There should be no tokens minted or burned.
    passert $(errCode AuctionEscrow'BidderBuys'Error'UnexpectedTokensMintedBurned) $
      pfromData txInfoFields.mint #== mempty

    -- (AUES14) This redeemer can only be used during
    -- the purchase period.
    passert $(errCode AuctionEscrow'BidderBuys'Error'IncorrectValidityInterval) $
      pcontains # (ppurchasePeriod # auctionTerms) # txInfoFields.validRange

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- (AUES15) There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- (AUES16) The auction escrow output should contain
    -- the auction escrow token.
    passert
      $(errCode AuctionEscrow'BidderBuys'Error'AuctionEscrowOutputMissingAuctionEscrowToken)
      (ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput)

    -- (AUES17) The auction escrow output should contain
    -- the standing bid token.
    passert
      $(errCode AuctionEscrow'BidderBuys'Error'AuctionEscrowOutputMissingStandingBidToken)
      (ptxOutContainsStandingBidToken # auctionCs # ownOutput)

    -- (AUES18) The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- (AUES19) The auction state should transition from
    -- `BiddingStarted` to `AuctionConcluded`.
    passert $(errCode AuctionEscrow'BidderBuys'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToAuctionConcluded
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check auction lot transfer to the winning bidder
    ----------------------------------------------------------------------------

    -- (AUES20) There should be exactly one standing bid input.
    standingBidInput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'MissingStandingBidInput)
          (pfindUniqueInputWithScriptHash # standingBidSh # txInfo)

    -- (AUES21) The standing bid input should contain the standing
    -- bid token.
    standingBidInputResolved <- plet $ pfield @"resolved" # standingBidInput
    passert $(errCode AuctionEscrow'BidderBuys'Error'StandingBidInputMissingToken) $
      (ptxOutContainsStandingBidToken # auctionCs # standingBidInputResolved)

    -- (AUES22) The standing bid output's datum should be decodable
    -- as a standing bid state.
    bidState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'BidderBuys'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum @PStandingBidState # standingBidInputResolved)

    -- (AUES23) The standing bid should contain bid terms.
    bidTerms <-
      plet $
        passertMaybeData
          $(errCode AuctionEscrow'BidderBuys'Error'EmptyStandingBid)
          (pto bidState)

    -- (AUES24) The bid terms in the standing bid input are valid.
    passert $(errCode AuctionEscrow'BidderBuys'Error'BidTermsInvalid) $
      pvalidateBidTerms # auctionCs # auctionTerms # bidTerms

    -- (AUES25) The auction lot is paid to the winning bidder,
    -- who is buying it.
    bidderPkh <- plet $ pfield @"biBidderPkh" #$ pfield @"btBidder" # bidTerms
    passert $(errCode AuctionEscrow'BidderBuys'Error'AuctionLotNotPaidToBidder) $
      auctionTermsFields.auctionLot #<= pvaluePaidTo # txInfo # bidderPkh

    -- (AUES26) The bidder signed the transaction.
    passert $(errCode AuctionEscrow'BidderBuys'Error'NoBidderConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata bidderPkh

    ----------------------------------------------------------------------------
    -- Check ADA payment to the seller
    ----------------------------------------------------------------------------

    -- (AUES27) The seller receives the proceeds of the auction.
    passert $(errCode AuctionEscrow'BidderBuys'Error'SellerPaymentIncorrect) $
      (psellerPayout # auctionTerms # bidTerms)
        #<= (plovelaceValueOf #$ pvaluePaidTo # txInfo # auctionTermsFields.sellerPkh)

    ----------------------------------------------------------------------------
    -- Check auction fees
    ----------------------------------------------------------------------------

    -- (AUES28) The total auction fees are sent to
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
          :--> PUnit
      )
pcheckSellerReclaims = phoistAcyclic $
  plam $ \feeEscrowSh txInfo auctionCs auctionTerms oldAuctionState ownAddress -> P.do
    txInfoFields <- pletFields @["mint", "signatories", "validRange"] txInfo
    auctionTermsFields <- pletFields @["auctionLot", "sellerPkh"] auctionTerms
    let sellerPkh = auctionTermsFields.sellerPkh

    -- (AUES29) There should be no tokens minted or burned.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'UnexpectedTokensMintedBurned) $
      pfromData txInfoFields.mint #== mempty

    -- (AUES30) This redeemer can only be used during
    -- the penalty period.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'IncorrectValidityInterval) $
      pcontains # (ppenaltyPeriod # auctionTerms) # txInfoFields.validRange

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- (AUES31) There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'SellerReclaims'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- (AUES32) The auction escrow output should contain
    -- the auction escrow token.
    passert
      $(errCode AuctionEscrow'SellerReclaims'Error'AuctionEscrowOutputMissingAuctionEscrowToken)
      (ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput)

    -- (AUES33) The auction escrow output should contain
    -- the standing bid token.
    passert
      $(errCode AuctionEscrow'SellerReclaims'Error'AuctionEscrowOutputMissingStandingBidToken)
      (ptxOutContainsStandingBidToken # auctionCs # ownOutput)

    -- (AUES34) The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'SellerReclaims'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- (AUES35) The auction state should transition from
    -- `BiddingStarted` to `AuctionConcluded`.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToAuctionConcluded
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check auction lot transfer back to the seller
    ----------------------------------------------------------------------------

    -- (AUES36) The auction lot is returned to the seller.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'PaymentToSellerIncorrect) $
      auctionTermsFields.auctionLot
        #<= (pvaluePaidTo # txInfo # sellerPkh)

    -- (AUES37) The seller signed the transaction.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'NoSellerConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata sellerPkh

    ----------------------------------------------------------------------------
    -- Check auction fees
    ----------------------------------------------------------------------------

    -- (AUES38) The total auction fees are sent to
    -- the fee escrow validator.
    passert $(errCode AuctionEscrow'SellerReclaims'Error'PaymentToFeeEscrowIncorrect) $
      (ptotalAuctionFees # auctionTerms)
        #<= (plovelaceValueOf #$ pvaluePaidToScript # txInfo # feeEscrowSh)

    pcon PUnit
