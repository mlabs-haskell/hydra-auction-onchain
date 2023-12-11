module HydraAuctionOnchain.Validators.StandingBid
  ( standingBidValidator
  ) where

import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueOutputWithAddress
  , putxoAddress
  )
import HydraAuctionOnchain.MintingPolicies.Auction (standingBidTokenName)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, pbiddingPeriod)
import HydraAuctionOnchain.Types.Error (ToErrorCode (toErrorCode), err, perrMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState, pvalidateNewBid)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PTxInInfo, PTxInfo, PTxOut)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.ScriptContext (ptryOwnInput, ptxSignedBy)
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
-- Errors
--------------------------------------------------------------------------------

data PStandingBidError (s :: S)
  = StandingBid'Error'OwnInputMissingToken
  | StandingBid'Error'UnexpectedTokensMintedBurned
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PStandingBidError where
  type DPTStrat _ = PlutusTypeScott

instance ToErrorCode PStandingBidError where
  toErrorCode = phoistAcyclic $
    plam $ \err -> pmatch err $ \case
      StandingBid'Error'OwnInputMissingToken ->
        pconstant "StandingBid01"
      StandingBid'Error'UnexpectedTokensMintedBurned ->
        pconstant "StandingBid02"

-- NewBid ----------------------------------------------------------------------
data PStandingBid'NewBid'Error (s :: S)
  = StandingBid'NewBid'Error'MissingOwnOutput
  | StandingBid'NewBid'Error'OwnOutputMissingToken
  | StandingBid'NewBid'Error'FailedToDecodeNewBid
  | StandingBid'NewBid'Error'InvalidNewBidState
  | StandingBid'NewBid'Error'IncorrectValidityInterval
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PStandingBid'NewBid'Error where
  type DPTStrat _ = PlutusTypeScott

instance ToErrorCode PStandingBid'NewBid'Error where
  toErrorCode = phoistAcyclic $
    plam $ \err -> pmatch err $ \case
      StandingBid'NewBid'Error'MissingOwnOutput ->
        pconstant "StandingBid_NewBid_01"
      StandingBid'NewBid'Error'OwnOutputMissingToken ->
        pconstant "StandingBid_NewBid_02"
      StandingBid'NewBid'Error'FailedToDecodeNewBid ->
        pconstant "StandingBid_NewBid_03"
      StandingBid'NewBid'Error'InvalidNewBidState ->
        pconstant "StandingBid_NewBid_04"
      StandingBid'NewBid'Error'IncorrectValidityInterval ->
        pconstant "StandingBid_NewBid_05"

-- MoveToHydra -----------------------------------------------------------------
data PStandingBid'MoveToHydra'Error (s :: S)
  = StandingBid'MoveToHydra'Error'MissingDelegateSignatures
  | StandingBid'MoveToHydra'Error'IncorrectValidityInterval
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PStandingBid'MoveToHydra'Error where
  type DPTStrat _ = PlutusTypeScott

instance ToErrorCode PStandingBid'MoveToHydra'Error where
  toErrorCode = phoistAcyclic $
    plam $ \err -> pmatch err $ \case
      StandingBid'MoveToHydra'Error'MissingDelegateSignatures ->
        pconstant "StandingBid_MoveToHydra_01"
      StandingBid'MoveToHydra'Error'IncorrectValidityInterval ->
        pconstant "StandingBid_MoveToHydra_02"

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
    ownInput <- plet $ ptryOwnInput # ctx
    txInfo <- plet $ pfield @"txInfo" # ctx

    -- (StandingBid01)
    -- The standing bid input should contain the standing bid token.
    err StandingBid'Error'OwnInputMissingToken $
      ptxOutContainsStandingBidToken # auctionCs #$ pfield @"resolved" # ownInput

    -- (StandingBid02)
    -- There should be no tokens minted or burned.
    mintValue <- plet $ pfield @"mint" # txInfo
    err StandingBid'Error'UnexpectedTokensMintedBurned $
      pfromData mintValue #== mempty

    pmatch redeemer $ \case
      NewBidRedeemer _ ->
        pcheckNewBid # txInfo # auctionCs # auctionTerms # ownInput # oldBidState
      MoveToHydraRedeemer _ ->
        pcheckMoveToHydra # txInfo # auctionTerms
      ConcludeAuctionRedeemer _ ->
        pcheckConcludeAuction

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
    -- (StandingBid_NewBid_01)
    -- The standing bid output should exist.
    ownOutput <-
      plet $
        perrMaybe
          # pcon StandingBid'NewBid'Error'MissingOwnOutput
          # (pfindUniqueOutputWithAddress # (putxoAddress # ownInput) # txInfo)

    -- (StandingBid_NewBid_02)
    -- The standing bid output should contain the standing bid token.
    err StandingBid'NewBid'Error'OwnOutputMissingToken $
      ptxOutContainsStandingBidToken # auctionCs # ownOutput

    -- (StandingBid_NewBid_03)
    -- The standing bid output's datum should be decodable
    -- as a standing bid state.
    newBidState <-
      plet $
        perrMaybe
          # pcon StandingBid'NewBid'Error'FailedToDecodeNewBid
          # (pdecodeInlineDatum # ownOutput)

    -- (StandingBid_NewBid_04)
    -- The transition from the old bid state to the new bid state
    -- should be valid.
    err StandingBid'NewBid'Error'InvalidNewBidState $
      pvalidateNewBid # auctionCs # auctionTerms # oldBidState # newBidState

    -- (StandingBid_NewBid_05)
    -- The transaction validity should end before the bidding end time.
    txInfoValidRange <- plet $ pfield @"validRange" # txInfo
    err StandingBid'NewBid'Error'IncorrectValidityInterval $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoValidRange

    pcon PUnit

--------------------------------------------------------------------------------
-- MoveToHydra
--------------------------------------------------------------------------------

pcheckMoveToHydra :: Term s (PTxInfo :--> PAuctionTerms :--> PUnit)
pcheckMoveToHydra = phoistAcyclic $
  plam $ \txInfo auctionTerms -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- (StandingBid_MoveToHydra_01)
    -- The transaction should be signed by all the delegates.
    delegates <- plet $ pfield @"delegates" # auctionTerms
    err StandingBid'MoveToHydra'Error'MissingDelegateSignatures $
      pall # plam (\sig -> ptxSignedBy # txInfoFields.signatories # sig) # delegates

    -- (StandingBid_MoveToHydra_02)
    -- The transaction validity should end before the bidding end time.
    err StandingBid'MoveToHydra'Error'IncorrectValidityInterval $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoFields.validRange

    pcon PUnit

--------------------------------------------------------------------------------
-- ConcludeAuction
--------------------------------------------------------------------------------

pcheckConcludeAuction :: Term s PUnit
pcheckConcludeAuction = undefined

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

ptxOutContainsStandingBidToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsStandingBidToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # standingBidTokenName)
      #== 1
