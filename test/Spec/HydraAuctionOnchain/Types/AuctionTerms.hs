{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.HydraAuctionOnchain.Types.AuctionTerms
  ( AuctionTerms (..)
  , biddingPeriod
  ) where

import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusLedgerApi.V2 (BuiltinByteString, POSIXTime, POSIXTimeRange, PubKeyHash, Value)
import PlutusTx (makeIsDataIndexed)
import Spec.HydraAuctionOnchain.Helpers (intervalFiniteClosedOpen)

data AuctionTerms = AuctionTerms
  { at'AuctionLot :: Value
  , at'SellerPkh :: PubKeyHash
  , at'SellerVk :: BuiltinByteString
  , at'Delegates :: [PubKeyHash]
  , at'BiddingStart :: POSIXTime
  , at'BiddingEnd :: POSIXTime
  , at'PurchaseDeadline :: POSIXTime
  , at'Cleanup :: POSIXTime
  , at'AuctionFeePerDelegate :: Integer
  , at'StartingBid :: Integer
  , at'MinBidIncrement :: Integer
  , at'MinDepositAmount :: Integer
  }
  deriving stock (Show, Eq)

makeIsDataIndexed ''AuctionTerms [('AuctionTerms, 0)]

deriving via
  (DerivePConstantViaData AuctionTerms PAuctionTerms)
  instance
    (PConstantDecl AuctionTerms)

instance PUnsafeLiftDecl PAuctionTerms where
  type PLifted PAuctionTerms = AuctionTerms

--------------------------------------------------------------------------------
-- Auction Lifecycle
--------------------------------------------------------------------------------

biddingPeriod :: AuctionTerms -> POSIXTimeRange
biddingPeriod AuctionTerms {..} = intervalFiniteClosedOpen at'BiddingStart at'BiddingEnd
