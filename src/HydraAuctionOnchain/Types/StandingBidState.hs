module HydraAuctionOnchain.Types.StandingBidState
  ( PStandingBidState (PStandingBidState)
  ) where

import HydraAuctionOnchain.Types.BidTerms (PBidTerms)
import Plutarch.Api.V2 (PMaybeData)

newtype PStandingBidState (s :: S) = PStandingBidState (Term s (PMaybeData PBidTerms))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PStandingBidState where
  type DPTStrat _ = PlutusTypeNewtype
