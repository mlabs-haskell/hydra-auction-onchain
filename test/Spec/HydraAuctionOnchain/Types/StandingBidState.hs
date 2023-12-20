{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.HydraAuctionOnchain.Types.StandingBidState
  ( StandingBidState (StandingBidState)
  ) where

import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusTx.IsData.Class (FromData, ToData)
import Spec.HydraAuctionOnchain.Types.BidTerms (BidTerms)

newtype StandingBidState = StandingBidState (Maybe BidTerms)
  deriving stock (Show, Eq)
  deriving newtype (FromData, ToData)

deriving via
  (DerivePConstantViaData StandingBidState PStandingBidState)
  instance
    (PConstantDecl StandingBidState)

instance PUnsafeLiftDecl PStandingBidState where
  type PLifted PStandingBidState = StandingBidState
