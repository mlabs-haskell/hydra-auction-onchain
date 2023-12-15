{-# LANGUAGE PackageImports #-}

module HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUnique
  , pfindUniqueInputWithToken
  , pfindUniqueOutputWithAddress
  , pgetOwnInput
  , pintervalFiniteClosedOpen
  , pserialise
  , putxoAddress
  ) where

import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
  ( PAddress
  , PCurrencySymbol
  , PExtended (PFinite)
  , PInterval (PInterval)
  , PLowerBound (PLowerBound)
  , POutputDatum (POutputDatum)
  , PScriptContext
  , PScriptPurpose (PSpending)
  , PTokenName
  , PTxInInfo
  , PTxInfo
  , PTxOut
  , PUpperBound (PUpperBound)
  )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Monadic qualified as P
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfromSingleton)

pdecodeInlineDatum :: PTryFrom PData a => Term s (PTxOut :--> PMaybe a)
pdecodeInlineDatum = phoistAcyclic $
  plam $ \txOut -> P.do
    datum <- plet $ pfield @"datum" # txOut
    pmatch datum $ \case
      POutputDatum inlineDatum ->
        pjust #$ pfromPDatum #$ pfield @"outputDatum" # inlineDatum
      _ -> pnothing

pfindUnique :: PIsListLike l a => Term s ((a :--> PBool) :--> l a :--> PMaybe a)
pfindUnique = phoistAcyclic $
  plam $ \predicate list ->
    pfromSingleton #$ pfilter # predicate # list

pfindUniqueInputWithToken
  :: Term
      s
      ( PCurrencySymbol
          :--> PTokenName
          :--> PTxInfo
          :--> PMaybe PTxInInfo
      )
pfindUniqueInputWithToken = phoistAcyclic $
  plam $ \cs tn txInfo ->
    pfindUnique
      # plam
        ( \utxo ->
            pvalueOf # (pfield @"value" #$ pfield @"resolved" # utxo) # cs # tn #== 1
        )
      #$ pfield @"inputs"
      # txInfo

pfindUniqueOutputWithAddress :: Term s (PAddress :--> PTxInfo :--> PMaybe PTxOut)
pfindUniqueOutputWithAddress = phoistAcyclic $
  plam $ \addr txInfo ->
    pfindUnique
      # plam (\out -> (pfield @"address" # out) #== addr)
      #$ pfield @"outputs"
      # txInfo

pgetOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
pgetOwnInput = phoistAcyclic $
  plam $ \ctx ->
    pmatch (pfield @"purpose" # ctx) $ \case
      PSpending rec -> P.do
        ownOutRef <- plet $ pfield @"_0" # rec
        inputs <- plet $ pfromData $ pfield @"inputs" #$ pfield @"txInfo" # ctx
        pfind
          # plam (\utxo -> pfield @"outRef" # utxo #== ownOutRef)
          # inputs
      _ -> pnothing

pintervalFiniteClosedOpen :: PIsData a => Term s (a :--> a :--> PInterval a)
pintervalFiniteClosedOpen = phoistAcyclic $
  plam $ \a b ->
    pcon $
      PInterval $
        pdcons @"from"
          # ( pdata $
                pcon $
                  PLowerBound $
                    pdcons @"_0"
                      # (pdata $ pcon $ PFinite $ pdcons @"_0" # pdata a # pdnil)
                      #$ pdcons @"_1"
                      # pconstantData True
                      # pdnil
            )
          #$ pdcons @"to"
          # ( pdata $
                pcon $
                  PUpperBound $
                    pdcons @"_0"
                      # (pdata $ pcon $ PFinite $ pdcons @"_0" # pdata b # pdnil)
                      #$ pdcons @"_1"
                      # pconstantData False
                      # pdnil
            )
          # pdnil

pserialise :: PIsData a => Term s (a :--> PByteString)
pserialise = phoistAcyclic $ plam $ \x -> pserialiseData #$ pforgetData $ pdata x

putxoAddress :: Term s (PTxInInfo :--> PAddress)
putxoAddress = phoistAcyclic $
  plam $ \utxo ->
    pfield @"address" #$ pfield @"resolved" # utxo
