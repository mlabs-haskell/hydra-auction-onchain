{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.HydraAuctionOnchain.Types.Redeemers
  ( AuctionEscrowRedeemer
      ( StartBiddingRedeemer
      , BidderBuysRedeemer
      , SellerReclaimsRedeemer
      , CleanupAuctionRedeemer
      )
  , StandingBidRedeemer (NewBidRedeemer, MoveToHydraRedeemer, ConcludeAuctionRedeemer)
  ) where

import HydraAuctionOnchain.Validators.AuctionEscrow (PAuctionEscrowRedeemer)
import HydraAuctionOnchain.Validators.StandingBid (PStandingBidRedeemer)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import PlutusTx (makeIsDataIndexed)

--------------------------------------------------------------------------------
-- AuctionEscrowRedeemer
--------------------------------------------------------------------------------

data AuctionEscrowRedeemer
  = StartBiddingRedeemer
  | BidderBuysRedeemer
  | SellerReclaimsRedeemer
  | CleanupAuctionRedeemer
  deriving stock (Show, Eq)

makeIsDataIndexed
  ''AuctionEscrowRedeemer
  [ ('StartBiddingRedeemer, 0)
  , ('BidderBuysRedeemer, 1)
  , ('SellerReclaimsRedeemer, 2)
  , ('CleanupAuctionRedeemer, 3)
  ]

deriving via
  (DerivePConstantViaData AuctionEscrowRedeemer PAuctionEscrowRedeemer)
  instance
    (PConstantDecl AuctionEscrowRedeemer)

instance PUnsafeLiftDecl PAuctionEscrowRedeemer where
  type PLifted PAuctionEscrowRedeemer = AuctionEscrowRedeemer

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
