{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.BidderDeposit
  ( PBidderDepositRedeemer
      ( UseDepositWinnerRedeemer
      , ReclaimDepositLoserRedeemer
      , DepositReclaimedAuctionConcludedRedeemer
      , DepositCleanupRedeemer
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
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, ppostBiddingPeriod)
import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState, pbidderLost, pbidderWon)
import HydraAuctionOnchain.Types.Tokens
  ( ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  )
import HydraAuctionOnchain.Validators.AuctionEscrow
  ( PAuctionEscrowRedeemer (BidderBuysRedeemer)
  )
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import Plutarch.Monadic qualified as P

----------------------------------------------------------------------
-- Redeemers

data PBidderDepositRedeemer (s :: S)
  = UseDepositWinnerRedeemer (Term s (PDataRecord '[]))
  | ReclaimDepositLoserRedeemer (Term s (PDataRecord '[]))
  | DepositReclaimedAuctionConcludedRedeemer (Term s (PDataRecord '[]))
  | DepositCleanupRedeemer (Term s (PDataRecord '[]))
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
      ( PScriptHash
          :--> PScriptHash
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
      _ -> undefined

----------------------------------------------------------------------
-- UseDepositWinner
--
-- Deposit is used by the bidder who won the auction to buy
-- the auction lot.

pcheckUseDepositWinner
  :: Term
      s
      ( PTxInfo
          :--> PScriptHash
          :--> PScriptHash
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
          (pfindUniqueInputWithScriptHash # standingBidSh # txInfo)

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
          (pdecodeInlineDatum @PStandingBidState # standingBidInputResolved)

    -- The bidder deposit's bidder won the auction.
    passert $(errCode BidderDeposit'UseDepositWinner'Error'BidderNotWinner) $
      pbidderWon # bidState # bidderInfo

    -- There should be exactly one auction escrow input.
    auctionEscrowInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'UseDepositWinner'Error'MissingAuctionEscrowInput)
          (pfindUniqueInputWithScriptHash # auctionEscrowSh # txInfo)

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
          :--> PScriptHash
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PBidderInfo
          :--> PUnit
      )
pcheckReclaimDepositLoser = phoistAcyclic $
  plam $ \txInfo standingBidSh auctionCs auctionTerms bidderInfo -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- There should be exactly one standing bid reference input.
    standingBidInput <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositLoser'Error'MissingStandingBidInput)
          (pfindUniqueRefInputWithScriptHash # standingBidSh # txInfo)

    -- The standing bid reference input should contain the standing
    -- bid token.
    standingBidInputResolved <- plet $ pfield @"resolved" # standingBidInput
    passert $(errCode BidderDeposit'ReclaimDepositLoser'Error'StandingBidInputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs # standingBidInputResolved

    -- The standing bid input contains a datum that can be decoded
    -- as a standing bid state.
    bidState <-
      plet $
        passertMaybe
          $(errCode BidderDeposit'ReclaimDepositLoser'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum @PStandingBidState # standingBidInputResolved)

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
