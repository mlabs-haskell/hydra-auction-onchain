module HydraAuctionOnchain.Lib.Value
  ( pvaluePositive
  ) where

import Plutarch.Api.V1.AssocMap qualified as AssocMap (pall)
import Plutarch.Api.V2 (PValue)

pvaluePositive :: Term s (PValue anyKey anyAmount :--> PBool)
pvaluePositive = phoistAcyclic $
  plam $ \value ->
    AssocMap.pall
      # plam (\submap -> AssocMap.pall # plam (0 #<) # submap)
      # pto value
