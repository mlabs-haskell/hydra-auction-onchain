{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.DelegateGroupMetadata
  ( delegateGroupMetadataValidator
  ) where

import HydraAuctionOnchain.Errors.Validators.DelegateGroupMetadata
  ( PDelegateGroupMetadataError (..)
  )
import HydraAuctionOnchain.Types.DelegateGroupInfo (PDelegateGroupInfo)
import HydraAuctionOnchain.Types.Error (errCode, passert)
import HydraAuctionOnchain.Types.Tokens
  ( delegateGroupTokenName
  , ptxOutContainsDelegateGroupToken
  )
import Plutarch.Api.V1.AssocMap qualified as Map (plookup, psingleton)
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Extra.Maybe (pjust)
import Plutarch.Extra.ScriptContext (ptryOwnInput)
import Plutarch.Monadic qualified as P

data PDelegateGroupMetadataRedeemer (s :: S)
  = RetireDelegateGroupRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PDelegateGroupMetadataRedeemer where
  type DPTStrat _ = PlutusTypeData

delegateGroupMetadataValidator
  :: Term
      s
      ( PDelegateGroupInfo
          :--> PDelegateGroupMetadataRedeemer
          :--> PScriptContext
          :--> PUnit
      )
delegateGroupMetadataValidator = phoistAcyclic $
  plam $ \delegateGroupInfo redeemer ctx ->
    pmatch redeemer $ \case
      RetireDelegateGroupRedeemer _ ->
        pcheckRetireDelegateGroup
          # delegateGroupInfo
          # ctx

pcheckRetireDelegateGroup
  :: Term
      s
      ( PDelegateGroupInfo
          :--> PScriptContext
          :--> PUnit
      )
pcheckRetireDelegateGroup = phoistAcyclic $
  plam $ \delegateGroupInfo ctx -> P.do
    delegateGroupCs <- plet $ pfield @"delegateGroupId" # delegateGroupInfo

    -- The metadata output must contain exactly one delegate group
    -- metadata token.
    delegateGroupMetadataOutput <- plet $ pfield @"resolved" #$ ptryOwnInput # ctx
    passert $(errCode DelegateGroupMetadata'Remove'Error'MetadataOutputMissingToken) $
      ptxOutContainsDelegateGroupToken
        # delegateGroupCs
        # delegateGroupMetadataOutput

    -- The delegate group metadata token should be burned.
    -- No other tokens should be minted or burned.
    mintValue <- plet $ pnormalize #$ pfield @"mint" #$ pfield @"txInfo" # ctx
    passert $(errCode DelegateGroupMetadata'Remove'Error'DelegateGroupTokenNotBurned) $
      (Map.plookup # delegateGroupCs # pto mintValue)
        #== (pjust #$ Map.psingleton # delegateGroupTokenName # (-1))

    pcon PUnit
