{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.BidderDeposit
  ( PBidderDepositRedeemer
      ( UseDepositWinnerRedeemer
      , DepositReclaimedByLoserRedeemer
      , DepositReclaimedAuctionConcludedRedeemer
      , DepositCleanupRedeemer
      )
  , bidderDepositValidator
  ) where

import HydraAuctionOnchain.Errors.Validators.BidderDeposit (PBidderDepositError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueInputWithScriptHash
  , pgetOwnInput
  , ponlyOneInputFromAddress
  , putxoAddress
  )
import HydraAuctionOnchain.Lib.ScriptContext (pinputSpentWithRedeemer)
import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState, pbidderWon)
import HydraAuctionOnchain.Types.Tokens
  ( ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  )
import HydraAuctionOnchain.Validators.AuctionEscrow
  ( PAuctionEscrowRedeemer (BidderBuysRedeemer)
  )
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo)
import Plutarch.Monadic qualified as P

----------------------------------------------------------------------
-- Redeemers

data PBidderDepositRedeemer (s :: S)
  = UseDepositWinnerRedeemer (Term s (PDataRecord '[]))
  | DepositReclaimedByLoserRedeemer (Term s (PDataRecord '[]))
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
          :--> PBidderInfo
          :--> PBidderDepositRedeemer
          :--> PScriptContext
          :--> PUnit
      )
bidderDepositValidator = phoistAcyclic $
  plam $ \standingBidSh auctionEscrowSh auctionCs bidderInfo redeemer ctx -> P.do
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
      _ -> undefined

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
