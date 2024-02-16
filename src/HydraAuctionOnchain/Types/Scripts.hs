module HydraAuctionOnchain.Types.Scripts
  ( PAuctionEscrowScriptHash (PAuctionEscrowScriptHash)
  , PFeeEscrowScriptHash (PFeeEscrowScriptHash)
  , PStandingBidScriptHash (PStandingBidScriptHash)
  ) where

import Plutarch.Api.V2 (PScriptHash)
import Ply.Plutarch (PlyArgOf)

----------------------------------------------------------------------
-- AuctionEscrow

newtype PAuctionEscrowScriptHash (s :: S) = PAuctionEscrowScriptHash (Term s PScriptHash)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PAuctionEscrowScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

data AuctionEscrowScriptHash

type instance PlyArgOf PAuctionEscrowScriptHash = AuctionEscrowScriptHash

----------------------------------------------------------------------
-- StandingBid

newtype PStandingBidScriptHash (s :: S) = PStandingBidScriptHash (Term s PScriptHash)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PStandingBidScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

data StandingBidScriptHash

type instance PlyArgOf PStandingBidScriptHash = StandingBidScriptHash

----------------------------------------------------------------------
-- FeeEscrow

newtype PFeeEscrowScriptHash (s :: S) = PFeeEscrowScriptHash (Term s PScriptHash)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PFeeEscrowScriptHash where
  type DPTStrat _ = PlutusTypeNewtype

data FeeEscrowScriptHash

type instance PlyArgOf PFeeEscrowScriptHash = FeeEscrowScriptHash
