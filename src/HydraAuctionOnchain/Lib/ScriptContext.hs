module HydraAuctionOnchain.Lib.ScriptContext
  ( pinputSpentWithRedeemer
  , pownCurrencySymbol
  ) where

import Plutarch.Api.V2
  ( PCurrencySymbol
  , PScriptContext
  , PScriptPurpose (PMinting, PSpending)
  , PTxInInfo
  , PTxInfo
  )
import Plutarch.Extra.Maybe (pjust, pmaybe, pnothing)
import Plutarch.Extra.ScriptContext (ptryFromRedeemer)
import Plutarch.Monadic qualified as P

pinputSpentWithRedeemer
  :: ( PIsData red
     , PTryFrom PData (PAsData red)
     )
  => Term
      s
      ( (red :--> PBool)
          :--> PTxInfo
          :--> PTxInInfo
          :--> PBool
      )
pinputSpentWithRedeemer = phoistAcyclic $
  plam $ \p txInfo input -> P.do
    redeemers <- plet $ pfield @"redeemers" # txInfo
    oref <- plet $ pfield @"outRef" # input
    purpose <- plet $ pcon $ PSpending $ pdcons @"_0" # oref # pdnil
    redeemer <- plet $ ptryFromRedeemer # purpose # redeemers
    pmaybe
      # pcon PFalse
      # plam (\redeemer -> p # pfromData redeemer)
      # redeemer

pownCurrencySymbol :: Term s (PScriptContext :--> PMaybe PCurrencySymbol)
pownCurrencySymbol = phoistAcyclic $
  plam $ \ctx ->
    pmatch (pfield @"purpose" # ctx) $ \case
      PMinting rec ->
        pjust #$ pfield @"_0" # rec
      _ ->
        pnothing
