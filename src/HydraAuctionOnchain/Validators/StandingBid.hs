{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.StandingBid
  ( standingBidValidator
  ) where

import HydraAuctionOnchain.Errors.StandingBid (PStandingBidError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueInputWithToken
  , pfindUniqueOutputWithAddress
  , pgetOwnInput
  , putxoAddress
  )
import HydraAuctionOnchain.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, pbiddingPeriod)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState, pvalidateNewBid)
import HydraAuctionOnchain.Validators.AuctionEscrow (pisConcluding)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
  ( PCurrencySymbol
  , PScriptContext
  , PScriptPurpose (PSpending)
  , PTxInInfo
  , PTxInfo
  , PTxOut
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
    -- (STBD0) The validator's own input should exist.
    ownInput <-
      plet $
        passertMaybe
          $(errCode StandingBid'Error'MissingStandingBidInput)
          (pgetOwnInput # ctx)

    txInfo <- plet $ pfield @"txInfo" # ctx

    -- (STBD1) The standing bid input should contain the standing
    -- bid token.
    passert $(errCode StandingBid'Error'OwnInputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs #$ pfield @"resolved" # ownInput

    -- (STBD2) There should be no tokens minted or burned.
    mintValue <- plet $ pfield @"mint" # txInfo
    passert $(errCode StandingBid'Error'UnexpectedTokensMintedBurned) $
      pfromData mintValue #== mempty

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
    -- (STBD3) The standing bid output should exist.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode StandingBid'NewBid'Error'MissingOwnOutput)
          (pfindUniqueOutputWithAddress # (putxoAddress # ownInput) # txInfo)

    -- (STBD4) The standing bid output should contain the standing
    -- bid token.
    passert $(errCode StandingBid'NewBid'Error'OwnOutputMissingToken) $
      ptxOutContainsStandingBidToken # auctionCs # ownOutput

    -- (STBD5) The standing bid output's datum should be decodable
    -- as a standing bid state.
    newBidState <-
      plet $
        passertMaybe
          $(errCode StandingBid'NewBid'Error'FailedToDecodeNewBid)
          (pdecodeInlineDatum # ownOutput)

    -- (STBD6) The transition from the old bid state to the new
    -- bid state should be valid.
    passert $(errCode StandingBid'NewBid'Error'InvalidNewBidState) $
      pvalidateNewBid # auctionCs # auctionTerms # oldBidState # newBidState

    -- (STBD7) The transaction validity should end before the
    -- bidding end time.
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

    -- (STBD8) The transaction should be signed by all the delegates.
    delegates <- plet $ pfield @"delegates" # auctionTerms
    passert $(errCode StandingBid'MoveToHydra'Error'MissingDelegateSignatures) $
      pall # plam (\sig -> ptxSignedBy # txInfoFields.signatories # sig) # delegates

    -- (STBD9) The transaction validity should end before the
    -- bidding end time.
    passert $(errCode StandingBid'MoveToHydra'Error'IncorrectValidityInterval) $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoFields.validRange

    pcon PUnit

--------------------------------------------------------------------------------
-- ConcludeAuction
--------------------------------------------------------------------------------

pcheckConcludeAuction :: Term s (PTxInfo :--> PCurrencySymbol :--> PUnit)
pcheckConcludeAuction = phoistAcyclic $
  plam $ \txInfo auctionCs -> P.do
    -- (STBD10) There is an input that contains
    -- the auction escrow token.
    auctionEscrowUtxo <-
      plet $
        passertMaybe
          $(errCode StandingBid'ConcludeAuction'Error'MissingAuctionEscrowInput)
          (pfindUniqueInputWithToken # auctionCs # auctionEscrowTokenName # txInfo)

    -- (STBD11) The auction escrow input is being spent with the
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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

ptxOutContainsStandingBidToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsStandingBidToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # standingBidTokenName)
      #== 1
