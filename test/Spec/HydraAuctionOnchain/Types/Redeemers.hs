{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.HydraAuctionOnchain.Types.Redeemers
  ( StandingBidRedeemer (NewBidRedeemer, MoveToHydraRedeemer, ConcludeAuctionRedeemer)
  ) where

import HydraAuctionOnchain.Validators.StandingBid (PStandingBidRedeemer)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusTx (makeIsDataIndexed)

--------------------------------------------------------------------------------
-- Standing bid validator
--------------------------------------------------------------------------------

data StandingBidRedeemer
  = NewBidRedeemer
  | MoveToHydraRedeemer
  | ConcludeAuctionRedeemer
  deriving stock (Show, Eq)

makeIsDataIndexed
  ''StandingBidRedeemer
  [ ('NewBidRedeemer, 0)
  , ('MoveToHydraRedeemer, 1)
  , ('ConcludeAuctionRedeemer, 2)
  ]

deriving via
  (DerivePConstantViaData StandingBidRedeemer PStandingBidRedeemer)
  instance
    (PConstantDecl StandingBidRedeemer)

instance PUnsafeLiftDecl PStandingBidRedeemer where
  type PLifted PStandingBidRedeemer = StandingBidRedeemer
