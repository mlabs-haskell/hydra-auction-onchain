{-# LANGUAGE PackageImports #-}

module HydraAuctionOnchain.Helpers
  ( pfindUnique
  , pfindUniqueOutputWithAddress
  , putxoAddress
  ) where

import Plutarch.Api.V2 (PAddress, PTxInInfo, PTxInfo, PTxOut)
import "liqwid-plutarch-extra" Plutarch.Extra.List (pfromSingleton)

pfindUnique :: PIsListLike l a => Term s ((a :--> PBool) :--> l a :--> PMaybe a)
pfindUnique = phoistAcyclic $
  plam $ \predicate list ->
    pfromSingleton #$ pfilter # predicate # list

pfindUniqueOutputWithAddress :: Term s (PAddress :--> PTxInfo :--> PMaybe PTxOut)
pfindUniqueOutputWithAddress = phoistAcyclic $
  plam $ \addr txInfo ->
    pfindUnique
      # plam (\out -> (pfield @"address" # out) #== addr)
      #$ pfield @"outputs"
      # txInfo

putxoAddress :: Term s (PTxInInfo :--> PAddress)
putxoAddress = phoistAcyclic $
  plam $ \utxo ->
    pfield @"address" #$ pfield @"resolved" # utxo
