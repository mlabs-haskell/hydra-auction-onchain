{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.HydraAuctionOnchain.Types.BidderInfo
  ( BidderInfo (..)
  ) where

import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V2 (BuiltinByteString, PubKeyHash)
import PlutusTx (makeIsDataIndexed)

data BidderInfo = BidderInfo
  { bi'BidderPkh :: PubKeyHash
  , bi'BidderVk :: BuiltinByteString
  }
  deriving stock (Show, Eq)

makeIsDataIndexed ''BidderInfo [('BidderInfo, 0)]

deriving via
  (DerivePConstantViaData BidderInfo PBidderInfo)
  instance
    (PConstantDecl BidderInfo)

instance PUnsafeLiftDecl PBidderInfo where
  type PLifted PBidderInfo = BidderInfo
