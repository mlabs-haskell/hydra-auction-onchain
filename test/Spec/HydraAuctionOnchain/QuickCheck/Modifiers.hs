{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Spec.HydraAuctionOnchain.QuickCheck.Modifiers
  ( GenNonAdaValue (GenNonAdaValue)
  ) where

import Data.Coerce (Coercible)
import Data.Functor.Identity (Identity (Identity))
import Plutarch.Test.QuickCheck.Modifiers (GenValue (GenValue))
import PlutusLedgerApi.V1.Value qualified as Value (getValue)
import PlutusLedgerApi.V2 (Value (Value), adaSymbol)
import PlutusTx.AssocMap qualified as AMap (delete)
import Test.QuickCheck (Arbitrary (arbitrary))

newtype GenNonAdaValue (mod :: Type -> Type) = GenNonAdaValue Value
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance
  ( Arbitrary (mod Integer)
  , forall (a :: Type). Coercible (mod a) a
  )
  => Arbitrary (GenNonAdaValue mod)
  where
  arbitrary = do
    GenValue valueWithAda <- arbitrary @(GenValue Identity mod)
    pure $ GenNonAdaValue $ Value $ AMap.delete adaSymbol $ Value.getValue valueWithAda
