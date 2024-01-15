{-# LANGUAGE PackageImports #-}

module HydraAuctionOnchain.Helpers
  ( paddressHasPubKeyHash
  , paddressHasScriptHash
  , pdecodeInlineDatum
  , pfindUnique
  , pfindUniqueInputWithScriptHash
  , pfindUniqueInputWithToken
  , pfindUniqueOutputWithAddress
  , pfindUniqueOutputWithScriptHash
  , pgetOwnInput
  , pintervalFiniteClosedOpen
  , ponlyOneInputFromAddress
  , pserialise
  , putxoAddress
  , pvaluePaidToAddr
  , pvaluePaidToScript
  ) where

import Plutarch.Api.V1.Address (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PAddress
  , PCurrencySymbol
  , PExtended (PFinite)
  , PInterval (PInterval)
  , PLowerBound (PLowerBound)
  , POutputDatum (POutputDatum)
  , PPubKeyHash
  , PScriptContext
  , PScriptHash
  , PScriptPurpose (PSpending)
  , PTokenName
  , PTxInInfo
  , PTxInfo
  , PTxOut
  , PUpperBound (PUpperBound)
  , PValue
  )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Monadic qualified as P
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfromSingleton)

paddressHasPubKeyHash :: Term s (PAddress :--> PPubKeyHash :--> PBool)
paddressHasPubKeyHash = phoistAcyclic $
  plam $ \addr pkh -> P.do
    credential <- plet $ pfield @"credential" #$ pto addr
    pmatch credential $ \case
      PPubKeyCredential rec -> pfield @"_0" # rec #== pkh
      PScriptCredential _ -> pcon PFalse

paddressHasScriptHash :: Term s (PAddress :--> PScriptHash :--> PBool)
paddressHasScriptHash = phoistAcyclic $
  plam $ \addr sh -> P.do
    credential <- plet $ pfield @"credential" #$ pto addr
    pmatch credential $ \case
      PScriptCredential rec -> pfield @"_0" # rec #== sh
      PPubKeyCredential _ -> pcon PFalse

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

pfindUniqueInputWithScriptHash :: Term s (PScriptHash :--> PTxInfo :--> PMaybe PTxInInfo)
pfindUniqueInputWithScriptHash = phoistAcyclic $
  plam $ \sh txInfo ->
    pfindUnique
      # plam
        ( \utxo -> P.do
            addr <- plet $ pfield @"address" #$ pfield @"resolved" # utxo
            paddressHasScriptHash # addr # sh
        )
      #$ pfield @"inputs"
      # txInfo

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

pfindUniqueOutputWithScriptHash :: Term s (PScriptHash :--> PTxInfo :--> PMaybe PTxOut)
pfindUniqueOutputWithScriptHash = phoistAcyclic $
  plam $ \sh txInfo ->
    pfindUnique
      # plam (\out -> paddressHasScriptHash # (pfield @"address" # out) # sh)
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

ponlyOneInputFromAddress :: Term s (PAddress :--> PTxInfo :--> PBool)
ponlyOneInputFromAddress = phoistAcyclic $
  plam $ \addr txInfo -> P.do
    inputs <- plet $ pfromData $ pfield @"inputs" # txInfo
    inputsFromAddress <-
      plet $
        pfilter
          # plam (\utxo -> (pfield @"address" #$ pfield @"resolved" # utxo) #== addr)
          # inputs
    plength # inputsFromAddress #== 1

pserialise :: PIsData a => Term s (a :--> PByteString)
pserialise = phoistAcyclic $ plam $ \x -> pserialiseData #$ pforgetData $ pdata x

putxoAddress :: Term s (PTxInInfo :--> PAddress)
putxoAddress = phoistAcyclic $
  plam $ \utxo ->
    pfield @"address" #$ pfield @"resolved" # utxo

pvaluePaidToAddr :: Term s (PTxInfo :--> PAddress :--> PValue 'Sorted 'Positive)
pvaluePaidToAddr =
  plam $ \txInfo addr ->
    pfoldl
      # plam
        ( \accValue utxo -> P.do
            utxoFields <- pletFields @["address", "value"] utxo
            pif (utxoFields.address #== addr) (accValue <> utxoFields.value) accValue
        )
      # mempty
      # (pfromData $ pfield @"outputs" # txInfo)

pvaluePaidToScript :: Term s (PTxInfo :--> PScriptHash :--> PValue 'Sorted 'Positive)
pvaluePaidToScript =
  plam $ \txInfo sh ->
    pfoldl
      # plam
        ( \accValue utxo -> P.do
            utxoFields <- pletFields @["address", "value"] utxo
            pif
              (paddressHasScriptHash # utxoFields.address # sh)
              (accValue <> utxoFields.value)
              accValue
        )
      # mempty
      # (pfromData $ pfield @"outputs" # txInfo)
