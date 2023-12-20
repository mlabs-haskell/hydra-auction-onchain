{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.StandingBid
  ( PStandingBidRedeemer (NewBidRedeemer, MoveToHydraRedeemer, ConcludeAuctionRedeemer)
  , standingBidValidator
  ) where

import HydraAuctionOnchain.Errors.StandingBid (PStandingBidError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueInputWithToken
  , pfindUniqueOutputWithAddress
  , pgetOwnInput
  , ponlyOneInputFromAddress
  , ptxOutContainsStandingBidToken
  , putxoAddress
  )
import HydraAuctionOnchain.MintingPolicies.Auction (auctionEscrowTokenName)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, pbiddingPeriod)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState, pvalidateNewBid)
import HydraAuctionOnchain.Validators.AuctionEscrow (pisConcluding)
import Plutarch.Api.V2
  ( PCurrencySymbol
  , PScriptContext
  , PScriptPurpose (PSpending)
  , PTxInInfo
  , PTxInfo
  )
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.Maybe (pmaybe)
import Plutarch.Extra.ScriptContext (ptryFromRedeemer, ptxSignedBy)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------
-- Redeemers
--------------------------------------------------------------------------------

data PStandingBidRedeemer (s :: S)
  = NewBidRedeemer (Term s (PDataRecord '[]))
  | MoveToHydraRedeemer (Term s (PDataRecord '[]))
  | ConcludeAuctionRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PStandingBidRedeemer where
  type DPTStrat _ = PlutusTypeData

--------------------------------------------------------------------------------
-- Validator
--------------------------------------------------------------------------------

standingBidValidator
  :: Term
      s
      ( PCurrencySymbol
          :--> PAuctionTerms
          :--> PStandingBidState
          :--> PStandingBidRedeemer
          :--> PScriptContext
          :--> PUnit
      )
standingBidValidator = phoistAcyclic $
  plam $ \auctionCs auctionTerms oldBidState redeemer ctx -> P.do
    txInfo <- plet $ pfield @"txInfo" # ctx

    -- (STBD0) The validator's own input should exist.
    ownInput <-
      plet $
        passertMaybe
          $(errCode StandingBid'Error'MissingStandingBidInput)
          (pgetOwnInput # ctx)

    -- (STBD1) There should only be one standing bid input.
    passert $(errCode StandingBid'Error'TooManyOwnScriptInputs) $
      ponlyOneInputFromAddress # (putxoAddress # ownInput) # txInfo

    -- (STBD2) The standing bid input should contain the standing
    -- bid token.
    passert $(errCode StandingBid'Error'OwnInputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs #$ pfield @"resolved" # ownInput

    -- (STBD3) There should be no tokens minted or burned.
    mintValue <- plet $ pfield @"mint" # txInfo
    passert $(errCode StandingBid'Error'UnexpectedTokensMintedBurned) $
      pfromData mintValue #== mempty

    -- Branching checks based on the redeemer used.
    pmatch redeemer $ \case
      NewBidRedeemer _ ->
        pcheckNewBid # txInfo # auctionCs # auctionTerms # ownInput # oldBidState
      MoveToHydraRedeemer _ ->
        pcheckMoveToHydra # txInfo # auctionTerms
      ConcludeAuctionRedeemer _ ->
        pcheckConcludeAuction # txInfo # auctionCs

--------------------------------------------------------------------------------
-- NewBid
--------------------------------------------------------------------------------

pcheckNewBid
  :: Term
      s
      ( PTxInfo
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PTxInInfo
          :--> PStandingBidState
          :--> PUnit
      )
pcheckNewBid = phoistAcyclic $
  plam $ \txInfo auctionCs auctionTerms ownInput oldBidState -> P.do
    -- (STBD4) The standing bid output should exist.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode StandingBid'NewBid'Error'MissingOwnOutput)
          (pfindUniqueOutputWithAddress # (putxoAddress # ownInput) # txInfo)

    -- (STBD5) The standing bid output should contain a standing
    -- bid token.
    passert $(errCode StandingBid'NewBid'Error'OwnOutputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs # ownOutput

    -- (STBD6) The standing bid output's datum should be decodable
    -- as a standing bid state.
    newBidState <-
      plet $
        passertMaybe
          $(errCode StandingBid'NewBid'Error'FailedToDecodeNewBid)
          (pdecodeInlineDatum # ownOutput)

    -- (STBD7) The transition from the old bid state to the new
    -- bid state should be valid.
    passert $(errCode StandingBid'NewBid'Error'InvalidNewBidState) $
      pvalidateNewBid # auctionCs # auctionTerms # oldBidState # newBidState

    -- (STBD8) This redeemer can only be used during
    -- the bidding period.
    txInfoValidRange <- plet $ pfield @"validRange" # txInfo
    passert $(errCode StandingBid'NewBid'Error'IncorrectValidityInterval) $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoValidRange

    pcon PUnit

--------------------------------------------------------------------------------
-- MoveToHydra
--------------------------------------------------------------------------------

pcheckMoveToHydra :: Term s (PTxInfo :--> PAuctionTerms :--> PUnit)
pcheckMoveToHydra = phoistAcyclic $
  plam $ \txInfo auctionTerms -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- (STBD9) The transaction should be signed by all the delegates.
    delegates <- plet $ pfield @"delegates" # auctionTerms
    passert $(errCode StandingBid'MoveToHydra'Error'MissingDelegateSignatures) $
      pall # plam (\sig -> ptxSignedBy # txInfoFields.signatories # sig) # delegates

    -- (STBD10) This redeemer can only be used during
    -- the bidding period.
    passert $(errCode StandingBid'MoveToHydra'Error'IncorrectValidityInterval) $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoFields.validRange

    pcon PUnit

--------------------------------------------------------------------------------
-- ConcludeAuction
--------------------------------------------------------------------------------

pcheckConcludeAuction :: Term s (PTxInfo :--> PCurrencySymbol :--> PUnit)
pcheckConcludeAuction = phoistAcyclic $
  plam $ \txInfo auctionCs -> P.do
    -- (STBD11) There is an input that contains
    -- the auction escrow token.
    auctionEscrowUtxo <-
      plet $
        passertMaybe
          $(errCode StandingBid'ConcludeAuction'Error'MissingAuctionEscrowInput)
          (pfindUniqueInputWithToken # auctionCs # auctionEscrowTokenName # txInfo)

    -- (STBD12) The auction escrow input is being spent with the
    -- `BidderBuys` or `SellerReclaims` redeemer. Implicitly, this
    -- means that the auction is concluding with either the winning
    -- bidder buying the auction lot or the seller reclaiming it.
    redeemers <- plet $ pfield @"redeemers" # txInfo
    auctionEscrowOref <- plet $ pfield @"outRef" # auctionEscrowUtxo
    spendsAuctionEscrow <- plet $ pcon $ PSpending $ pdcons @"_0" # auctionEscrowOref # pdnil
    auctionEscrowRedeemer <- plet $ ptryFromRedeemer # spendsAuctionEscrow # redeemers
    passert $(errCode StandingBid'ConcludeAuction'Error'InvalidAuctionEscrowRedeemer) $
      pmaybe
        # pcon PFalse
        # plam (\redeemer -> pisConcluding # pfromData redeemer)
        # auctionEscrowRedeemer

    pcon PUnit
