module HydraAuctionOnchain.Lib.ScriptContext
  ( pownCurrencySymbol
  ) where

import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PScriptPurpose (PMinting))
import Plutarch.Extra.Maybe (pjust, pnothing)

pownCurrencySymbol :: Term s (PScriptContext :--> PMaybe PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $
  plam $ \ctx ->
    pmatch (pfield @"purpose" # ctx) $ \case
      PMinting rec ->
        pjust #$ pfield @"_0" # rec
      _ ->
        pnothing
