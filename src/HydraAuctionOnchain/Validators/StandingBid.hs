module HydraAuctionOnchain.Validators.StandingBid
  ( standingBidValidator
  ) where

import HydraAuctionOnchain.Helpers (pfindUniqueOutputWithAddress, putxoAddress)
import HydraAuctionOnchain.MintingPolicies.Auction (standingBidTokenName)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import HydraAuctionOnchain.Types.Error (ToErrorCode (toErrorCode), err, perrMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PTxInInfo, PTxInfo)
import Plutarch.Extra.ScriptContext (ptryOwnInput)
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

data PStandingBid'NewBid'Error (s :: S)
  = StandingBid'NewBid'Error'MissingOwnOutput
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PStandingBid'NewBid'Error where
  type DPTStrat _ = PlutusTypeScott

instance ToErrorCode PStandingBid'NewBid'Error where
  toErrorCode = phoistAcyclic $
    plam $ \err -> pmatch err $ \case
      StandingBid'NewBid'Error'MissingOwnOutput ->
        pconstant "StandingBid_NewBid_01"

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
  plam $ \auctionCs _auctionTerms _standingBidState redeemer ctx -> P.do
    ownInput <- plet $ ptryOwnInput # ctx
    txInfo <- plet $ pfield @"txInfo" # ctx

    -- (StandingBid01)
    -- The standing bid input should contain the standing bid token.
    utxoValue <- plet $ pfield @"value" #$ pfield @"resolved" # ownInput
    err StandingBid'Error'OwnInputMissingToken $
      (pvalueOf # utxoValue # auctionCs # standingBidTokenName) #== 1

    -- (StandingBid02)
    -- There should be no tokens minted or burned.
    mintValue <- plet $ pfield @"mint" # txInfo
    err StandingBid'Error'UnexpectedTokensMintedBurned $
      pfromData mintValue #== mempty

    pmatch redeemer $ \case
      NewBidRedeemer _ -> checkNewBid # txInfo # ownInput
      MoveToHydraRedeemer _ -> checkMoveToHydra
      ConcludeAuctionRedeemer _ -> checkConcludeAuction

checkNewBid :: Term s (PTxInfo :--> PTxInInfo :--> PUnit)
checkNewBid = phoistAcyclic $
  plam $ \txInfo ownInput -> P.do
    -- (StandingBid_NewBid_01)
    -- The standing bid output should exist.
    _ownOutput <-
      plet $
        perrMaybe
          # pcon StandingBid'NewBid'Error'MissingOwnOutput
          # (pfindUniqueOutputWithAddress # (putxoAddress # ownInput) # txInfo)
    undefined

checkMoveToHydra :: Term s PUnit
checkMoveToHydra = undefined

checkConcludeAuction :: Term s PUnit
checkConcludeAuction = undefined
