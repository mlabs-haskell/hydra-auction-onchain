module HydraAuctionOnchain.Types.Error
  ( ToErrorCode (toErrorCode)
  , err
  , perrMaybe
  ) where

import Plutarch.Extra.Bool (passert)

class ToErrorCode a where
  toErrorCode :: Term s (a :--> PString)

err :: (PlutusType e, ToErrorCode e) => e s -> Term s PBool -> Term s a -> Term s a
err e = passert (toErrorCode # pcon e)

perrMaybe :: ToErrorCode e => Term s (e :--> PMaybe a :--> a)
perrMaybe = phoistAcyclic $
  plam $ \err mval -> pmatch mval $ \case
    PJust val -> val
    PNothing -> ptraceError $ toErrorCode # err
