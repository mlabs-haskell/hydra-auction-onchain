{-# LANGUAGE PackageImports #-}

module HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUnique
  , pfindUniqueInputWithToken
  , pfindUniqueOutputWithAddress
  , pfindUniqueOutputWithScriptHash
  , pgetOwnInput
  , pintervalFiniteClosedOpen
  , ponlyOneInputFromAddress
  , pserialise
  , ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  , putxoAddress
  ) where

import HydraAuctionOnchain.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
  ( PAddress
  , PCurrencySymbol
  , PExtended (PFinite)
  , PInterval (PInterval)
  , PLowerBound (PLowerBound)
  , POutputDatum (POutputDatum)
  , PScriptContext
  , PScriptHash
  , PScriptPurpose (PSpending)
  , PTokenName
  , PTxInInfo
  , PTxInfo
  , PTxOut
  , PUpperBound (PUpperBound)
  )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Extra.Maybe (pdnothing, pjust, pnothing)
import Plutarch.Extra.ScriptContext (paddressFromScriptHash, pfromPDatum)
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

pfindUniqueOutputWithScriptHash :: Term s (PScriptHash :--> PTxInfo :--> PMaybe PTxOut)
pfindUniqueOutputWithScriptHash = phoistAcyclic $
  plam $ \scriptHash txInfo ->
    pfindUniqueOutputWithAddress
      # (paddressFromScriptHash # scriptHash # pdnothing)
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

ptxOutContainsAuctionEscrowToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsAuctionEscrowToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # auctionEscrowTokenName)
      #== 1

ptxOutContainsStandingBidToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsStandingBidToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # standingBidTokenName)
      #== 1

putxoAddress :: Term s (PTxInInfo :--> PAddress)
putxoAddress = phoistAcyclic $
  plam $ \utxo ->
    pfield @"address" #$ pfield @"resolved" # utxo
