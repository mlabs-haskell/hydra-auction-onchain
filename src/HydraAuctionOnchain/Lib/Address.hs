module HydraAuctionOnchain.Lib.Address
  ( paddrPaymentKeyHash
  , paddrPaymentKeyHashUnsafe
  ) where

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V2 (PAddress, PPubKeyHash)
import Plutarch.Extra.Maybe (pjust, pnothing)

paddrPaymentKeyHash :: Term s (PAddress :--> PMaybe PPubKeyHash)
paddrPaymentKeyHash = phoistAcyclic $
  plam $ \addr ->
    pmatch (pfield @"credential" # addr) $ \case
      PPubKeyCredential rec -> pjust #$ pfield @"_0" # rec
      PScriptCredential _ -> pnothing

paddrPaymentKeyHashUnsafe :: Term s (PAddress :--> PPubKeyHash)
paddrPaymentKeyHashUnsafe = phoistAcyclic $
  plam $ \addr ->
    pmatch (pfield @"credential" # addr) $ \case
      PPubKeyCredential rec -> pfield @"_0" # rec
      PScriptCredential _ ->
        ptraceError "paddrPaymentKeyHashUnsafe: failed to get payment pkh"
