{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.HydraAuctionOnchain.Types.BidTerms
  ( BidTerms (..)
  ) where

import HydraAuctionOnchain.Types.BidTerms (PBidTerms)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V2 (BuiltinByteString)
import PlutusTx (makeIsDataIndexed)
import Spec.HydraAuctionOnchain.Types.BidderInfo (BidderInfo)

data BidTerms = BidTerms
  { bt'Bidder :: BidderInfo
  , bt'BidPrice :: Integer
  , bt'BidderSignature :: BuiltinByteString
  , bt'SellerSignature :: BuiltinByteString
  }
  deriving stock (Show, Eq)

makeIsDataIndexed ''BidTerms [('BidTerms, 0)]

deriving via
  (DerivePConstantViaData BidTerms PBidTerms)
  instance
    (PConstantDecl BidTerms)

instance PUnsafeLiftDecl PBidTerms where
  type PLifted PBidTerms = BidTerms
