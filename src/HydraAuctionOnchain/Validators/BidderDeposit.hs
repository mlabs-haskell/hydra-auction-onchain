{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.BidderDeposit
  ( PBidderDepositRedeemer
      ( UseDepositWinnerRedeemer
      , ReclaimDepositLoserRedeemer
      , ReclaimDepositAuctionConcludedRedeemer
      , ReclaimDepositCleanupRedeemer
      )
  , bidderDepositValidator
  ) where

import HydraAuctionOnchain.Errors.Validators.BidderDeposit (PBidderDepositError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueInputWithScriptHash
  , pfindUniqueRefInputWithScriptHash
  , pgetOwnInput
  , ponlyOneInputFromAddress
  , putxoAddress
  )
import HydraAuctionOnchain.Lib.Address (paddrPaymentKeyHash)
import HydraAuctionOnchain.Lib.ScriptContext (pinputSpentWithRedeemer)
import HydraAuctionOnchain.Types.AuctionEscrowState (PAuctionEscrowState (AuctionConcluded))
import HydraAuctionOnchain.Types.AuctionTerms
  ( PAuctionTerms
  , pcleanupPeriod
  , ppostBiddingPeriod
  )
import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.Scripts (PAuctionEscrowScriptHash, PStandingBidScriptHash)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState, pbidderLost, pbidderWon)
import HydraAuctionOnchain.Types.Tokens
  ( ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  )
import HydraAuctionOnchain.Validators.AuctionEscrow
  ( PAuctionEscrowRedeemer (BidderBuysRedeemer)
  )
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PTxInfo)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import Plutarch.Monadic qualified as P

----------------------------------------------------------------------
-- Redeemers

data PBidderDepositRedeemer (s :: S)
  = UseDepositWinnerRedeemer (Term s (PDataRecord '[]))
  | ReclaimDepositLoserRedeemer (Term s (PDataRecord '[]))
  | ReclaimDepositAuctionConcludedRedeemer (Term s (PDataRecord '[]))
  | ReclaimDepositCleanupRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PBidderDepositRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PBidderDepositRedeemer)

----------------------------------------------------------------------
-- Validator

bidderDepositValidator
  :: Term
      s
      ( PStandingBidScriptHash
          :--> PAuctionEscrowScriptHash
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PBidderInfo
          :--> PBidderDepositRedeemer
          :--> PScriptContext
          :--> PUnit
      )
bidderDepositValidator = phoistAcyclic $
  plam $ \standingBidSh auctionEscrowSh auctionCs auctionTerms bidderInfo redeemer ctx -> P.do
    txInfo <- plet $ pfield @"txInfo" # ctx

    -- The validator's own input should exist.
    ownInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'Error'MissingOwnInput)
          (pgetOwnInput # ctx)

    -- There should be only one bidder deposit input.
    passert $(errCode BidderDeposit'Error'TooManyOwnScriptInputs) $
      ponlyOneInputFromAddress # (putxoAddress # ownInput) # txInfo

    -- No tokens are minted or burned.
    mintValue <- plet $ pfield @"mint" # txInfo
    passert $(errCode BidderDeposit'Error'UnexpectedTokensMintedBurned) $
      pfromData mintValue #== mempty

    -- Branching checks based on the redeemer used.
    pmatch redeemer $ \case
      UseDepositWinnerRedeemer _ ->
        pcheckUseDepositWinner
          # txInfo
          # standingBidSh
          # auctionEscrowSh
          # auctionCs
          # bidderInfo
      ReclaimDepositLoserRedeemer _ ->
        pcheckReclaimDepositLoser
          # txInfo
          # standingBidSh
          # auctionCs
          # auctionTerms
          # bidderInfo
      ReclaimDepositAuctionConcludedRedeemer _ ->
        pcheckReclaimDepositAuctionConcluded
          # txInfo
          # auctionEscrowSh
          # auctionCs
          # auctionTerms
          # bidderInfo
      ReclaimDepositCleanupRedeemer _ ->
        pcheckReclaimDepositCleanup
          # txInfo
          # auctionTerms
          # bidderInfo

----------------------------------------------------------------------
-- UseDepositWinner
--
-- Deposit is used by the bidder who won the auction to buy
-- the auction lot.

pcheckUseDepositWinner
  :: Term
      s
      ( PTxInfo
          :--> PStandingBidScriptHash
          :--> PAuctionEscrowScriptHash
          :--> PCurrencySymbol
          :--> PBidderInfo
          :--> PUnit
      )
pcheckUseDepositWinner = phoistAcyclic $
  plam $ \txInfo standingBidSh auctionEscrowSh auctionCs bidderInfo -> P.do
    -- There should be exactly one standing bid input.
    standingBidInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'UseDepositWinner'Error'MissingStandingBidInput)
          (pfindUniqueInputWithScriptHash # pto standingBidSh # txInfo)

    -- The standing bid input should contain the standing
    -- bid token.
    standingBidInputResolved <- plet $ pfield @"resolved" # standingBidInput
    passert $(errCode BidderDeposit'UseDepositWinner'Error'StandingBidInputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs # standingBidInputResolved

    -- The standing bid input contains a datum that can be decoded
    -- as a standing bid state.
    bidState <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'UseDepositWinner'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum # standingBidInputResolved)

    -- The bidder deposit's bidder won the auction.
    passert $(errCode BidderDeposit'UseDepositWinner'Error'BidderNotWinner) $
      pbidderWon # bidState # bidderInfo

    -- There should be exactly one auction escrow input.
    auctionEscrowInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'UseDepositWinner'Error'MissingAuctionEscrowInput)
          (pfindUniqueInputWithScriptHash # pto auctionEscrowSh # txInfo)

    -- The auction escrow input should contain the auction
    -- escrow token.
    auctionEscrowInputResolved <- plet $ pfield @"resolved" # auctionEscrowInput
    passert $(errCode BidderDeposit'UseDepositWinner'Error'AuctionEscrowInputMissingToken) $
      ptxOutContainsAuctionEscrowToken # auctionCs # auctionEscrowInputResolved

    -- The auction escrow input is being spent with the `BidderBuys`
    -- redeemer.
    passert $(errCode BidderDeposit'UseDepositWinner'Error'InvalidAuctionEscrowRedeemer) $
      pinputSpentWithRedeemer
        # plam (\redeemer -> redeemer #== pcon (BidderBuysRedeemer pdnil))
        # txInfo
        # auctionEscrowInput

    pcon PUnit

----------------------------------------------------------------------
-- ReclaimDepositLoser
--
-- The bidder deposit is reclaimed by a bidder that did not win
-- the auction.

pcheckReclaimDepositLoser
  :: Term
      s
      ( PTxInfo
          :--> PStandingBidScriptHash
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PBidderInfo
          :--> PUnit
      )
pcheckReclaimDepositLoser = phoistAcyclic $
  plam $ \txInfo standingBidSh auctionCs auctionTerms bidderInfo -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- There should be exactly one standing bid reference input.
    standingBidRefInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositLoser'Error'MissingStandingBidInput)
          (pfindUniqueRefInputWithScriptHash # pto standingBidSh # txInfo)

    -- The standing bid reference input should contain the standing
    -- bid token.
    standingBidRefInputResolved <- plet $ pfield @"resolved" # standingBidRefInput
    passert $(errCode BidderDeposit'ReclaimDepositLoser'Error'StandingBidInputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs # standingBidRefInputResolved

    -- The standing bid input contains a datum that can be decoded
    -- as a standing bid state.
    bidState <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositLoser'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum @PStandingBidState # standingBidRefInputResolved)

    -- The bidder deposit's bidder lost the auction.
    passert $(errCode BidderDeposit'ReclaimDepositLoser'Error'BidderNotLoser) $
      pbidderLost # bidState # bidderInfo

    -- This redeemer can only be used after the bidding period.
    passert $(errCode BidderDeposit'ReclaimDepositLoser'Error'IncorrectValidityInterval) $
      pcontains # (ppostBiddingPeriod # auctionTerms) # txInfoFields.validRange

    -- The payment part of the bidder address should be pkh.
    bidderPkh <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositLoser'Error'InvalidBidderAddress)
          (paddrPaymentKeyHash #$ pfield @"biBidderAddress" # bidderInfo)

    -- The bidder deposit's bidder signed the transaction.
    passert $(errCode BidderDeposit'ReclaimDepositLoser'Error'NoBidderConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata bidderPkh

    pcon PUnit

----------------------------------------------------------------------
-- ReclaimDepositAuctionConcluded
--
-- The bidder deposit is reclaimed by a bidder after the auction
-- conclusion. If the auction has concluded then the seller and the
-- winning bidder have already had an opportunity to claim
-- whichever deposits they are entitled to.

pcheckReclaimDepositAuctionConcluded
  :: Term
      s
      ( PTxInfo
          :--> PAuctionEscrowScriptHash
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PBidderInfo
          :--> PUnit
      )
pcheckReclaimDepositAuctionConcluded = phoistAcyclic $
  plam $ \txInfo auctionEscrowSh auctionCs auctionTerms bidderInfo -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- There should be exactly one auction escrow reference input.
    auctionEscrowRefInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositConcluded'Error'MissingAuctionRefInput)
          (pfindUniqueRefInputWithScriptHash # pto auctionEscrowSh # txInfo)

    -- The auction escrow reference input should contain the auction
    -- escrow token.
    auctionEscrowRefInputResolved <- plet $ pfield @"resolved" # auctionEscrowRefInput
    passert $(errCode BidderDeposit'ReclaimDepositConcluded'Error'AuctionRefInputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs # auctionEscrowRefInputResolved

    -- The auction escrow input contains a datum that can be
    -- decoded as an auction escrow state.
    auctionState <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositConcluded'Error'FailedToDecodeAuctionState)
          (pdecodeInlineDatum # auctionEscrowRefInputResolved)

    -- The auction is concluded.
    passert $(errCode BidderDeposit'ReclaimDepositConcluded'Error'AuctionNotConcluded) $
      auctionState #== pcon (AuctionConcluded pdnil)

    -- This redeemer can only be used after the bidding period.
    -- TODO: Remove this check? It is probably redundant since the
    -- auction can only conclude after the bidding end time, which is
    -- enforced by the auction escrow validator.
    passert $(errCode BidderDeposit'ReclaimDepositConcluded'Error'IncorrectValidityInterval) $
      pcontains # (ppostBiddingPeriod # auctionTerms) # txInfoFields.validRange

    -- The payment part of the bidder address should be pkh.
    bidderPkh <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositConcluded'Error'InvalidBidderAddress)
          (paddrPaymentKeyHash #$ pfield @"biBidderAddress" # bidderInfo)

    -- The bidder deposit's bidder signed the transaction.
    passert $(errCode BidderDeposit'ReclaimDepositConcluded'Error'NoBidderConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata bidderPkh

    pcon PUnit

----------------------------------------------------------------------
-- ReclaimDepositCleanup
--
-- If, for whatever reason, there are bidder deposits left during
-- the cleanup period, then whoever placed a deposit can freely
-- reclaim it.

pcheckReclaimDepositCleanup :: Term s (PTxInfo :--> PAuctionTerms :--> PBidderInfo :--> PUnit)
pcheckReclaimDepositCleanup = phoistAcyclic $
  plam $ \txInfo auctionTerms bidderInfo -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- This redeemer can only be used during the cleanup period.
    passert $(errCode BidderDeposit'ReclaimDepositCleanup'Error'IncorrectValidityInterval) $
      pcontains # (pcleanupPeriod # auctionTerms) # txInfoFields.validRange

    -- The payment part of the bidder address should be pkh.
    bidderPkh <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositCleanup'Error'InvalidBidderAddress)
          (paddrPaymentKeyHash #$ pfield @"biBidderAddress" # bidderInfo)

    -- The bidder deposit's bidder signed the transaction.
    passert $(errCode BidderDeposit'ReclaimDepositCleanup'Error'NoBidderConsent) $
      ptxSignedBy # txInfoFields.signatories # pdata bidderPkh

    pcon PUnit
