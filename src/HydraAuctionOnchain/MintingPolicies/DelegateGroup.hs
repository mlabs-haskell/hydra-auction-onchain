{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.MintingPolicies.DelegateGroup
  ( delegateGroupMintingPolicy
  ) where

import HydraAuctionOnchain.Errors.MintingPolicies.DelegateGroup (PDelegateGroupMpError (..))
import HydraAuctionOnchain.Helpers (pdecodeInlineDatum, pfindUniqueOutputWithScriptHash)
import HydraAuctionOnchain.Lib.ScriptContext (pownCurrencySymbol)
import HydraAuctionOnchain.Types.DelegateGroupInfo (PDelegateGroupInfo)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.Tokens
  ( delegateGroupTokenName
  , ptxOutContainsDelegateGroupToken
  )
import Plutarch.Api.V1.AssocMap qualified as Map (plookup, psingleton)
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PScriptHash, PTxOutRef)
import Plutarch.Extra.Maybe (pisJust, pjust)
import Plutarch.Extra.ScriptContext (pfindTxInByTxOutRef, ptxSignedBy)
import Plutarch.Monadic qualified as P

data PDelegateGroupMpRedeemer (s :: S)
  = MintDelegateGroupRedeemer (Term s (PDataRecord '[]))
  | BurnDelegateGroupRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PDelegateGroupMpRedeemer where
  type DPTStrat _ = PlutusTypeData

delegateGroupMintingPolicy
  :: Term
      s
      ( PScriptHash
          :--> PTxOutRef
          :--> PDelegateGroupMpRedeemer
          :--> PScriptContext
          :--> PUnit
      )
delegateGroupMintingPolicy = phoistAcyclic $
  plam $ \delegateGroupMetadataSh nonceOref redeemer ctx -> P.do
    -- Script purpose should contain own currency symbol. Always
    -- true for minting policies.
    delegateGroupCs <-
      plet $
        passertMaybe
          $(errCode DelegateGroupMp'Error'MissingOwnCurrencySymbol)
          (pownCurrencySymbol # ctx)

    pmatch redeemer $ \case
      MintDelegateGroupRedeemer _ ->
        pcheckMintDelegateGroup
          # delegateGroupMetadataSh
          # nonceOref
          # ctx
          # delegateGroupCs
      BurnDelegateGroupRedeemer _ ->
        pcheckBurnDelegateGroup
          # ctx
          # delegateGroupCs

pcheckMintDelegateGroup
  :: Term
      s
      ( PScriptHash
          :--> PTxOutRef
          :--> PScriptContext
          :--> PCurrencySymbol
          :--> PUnit
      )
pcheckMintDelegateGroup = phoistAcyclic $
  plam $ \delegateGroupMetadataSh nonceOref ctx delegateGroupCs -> P.do
    txInfo <- plet $ pfield @"txInfo" # ctx
    txInfoFields <- pletFields @["inputs", "mint", "signatories"] txInfo
    mintValue <- plet $ pnormalize # txInfoFields.mint

    -- The delegate group token should be minted.
    -- No other tokens should be minted or burned using this policy.
    passert $(errCode DelegateGroupMp'Mint'Error'DelegateGroupTokenNotMinted) $
      (Map.plookup # delegateGroupCs # pto mintValue)
        #== (pjust #$ Map.psingleton # delegateGroupTokenName # 1)

    -- The utxo nonce parameter of the minting policy should be a
    -- reference to a utxo input spent by the transaction.
    passert $(errCode DelegateGroupMp'Mint'Error'MissingUtxoNonceInput) $
      pisJust #$ pfindTxInByTxOutRef # nonceOref # txInfoFields.inputs

    -- There should be exactly one delegate group metadata output.
    delegateGroupMetadataOutput <-
      plet $
        passertMaybe
          $(errCode DelegateGroupMp'Mint'Error'MissingMetadataOutput)
          (pfindUniqueOutputWithScriptHash # delegateGroupMetadataSh # txInfo)

    -- The delegate group metadata output should contain a delegate group
    -- metadata token.
    passert $(errCode DelegateGroupMp'Mint'Error'MetadataOutputMissingToken) $
      ptxOutContainsDelegateGroupToken
        # delegateGroupCs
        # delegateGroupMetadataOutput

    -- The delegate group metadata output contains a datum that can be
    -- decoded as a delegate group info metadata record.
    (delegateGroupInfo :: Term s PDelegateGroupInfo) <-
      plet $
        passertMaybe
          $(errCode DelegateGroupMp'Mint'Error'FailedToDecodeDelegateGroupInfo)
          (pdecodeInlineDatum # delegateGroupMetadataOutput)

    -- The metadata record should contain an ID that matches the currency symbol
    -- of this minting policy.
    delegateGroupInfoFields <-
      pletFields
        @["delegateGroupId", "delegateGroupMasterKeys"]
        delegateGroupInfo
    passert $(errCode DelegateGroupMp'Mint'Error'DelegateGroupCurrencySymbolMismatch) $
      delegateGroupInfoFields.delegateGroupId #== delegateGroupCs

    -- The transaction should be signed by the delegate master keys.
    passert $(errCode DelegateGroupMp'Mint'Error'MissingDelegateSignatures) $
      pall
        # plam (\sig -> ptxSignedBy # txInfoFields.signatories # sig)
        # delegateGroupInfoFields.delegateGroupMasterKeys

    pcon PUnit

pcheckBurnDelegateGroup :: Term s (PScriptContext :--> PCurrencySymbol :--> PUnit)
pcheckBurnDelegateGroup = phoistAcyclic $
  plam $ \ctx delegateGroupCs -> P.do
    mintValue <- plet $ pnormalize #$ pfield @"mint" #$ pfield @"txInfo" # ctx

    -- The delegate group metadata token should be burned.
    -- No other tokens should be minted or burned.
    passert $(errCode DelegateGroupMp'Burn'Error'DelegateGroupTokenNotBurned) $
      (Map.plookup # delegateGroupCs # pto mintValue)
        #== (pjust #$ Map.psingleton # delegateGroupTokenName # (-1))

    pcon PUnit
