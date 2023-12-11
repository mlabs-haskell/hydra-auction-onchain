module HydraAuctionOnchain.Types.StandingBidState
  ( PStandingBidState (PStandingBidState)
  , pvalidateNewBid
  ) where

import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import HydraAuctionOnchain.Types.BidTerms (PBidTerms, pvalidateBidTerms)
import Plutarch.Api.V2 (PCurrencySymbol, PMaybeData)
import Plutarch.Extra.Maybe (pmaybeData)
import Plutarch.Monadic qualified as P

newtype PStandingBidState (s :: S) = PStandingBidState (Term s (PMaybeData PBidTerms))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PStandingBidState where
  type DPTStrat _ = PlutusTypeNewtype

instance PTryFrom PData PStandingBidState

pvalidateNewBid
  :: Term
      s
      ( PCurrencySymbol
          :--> PAuctionTerms
          :--> PStandingBidState
          :--> PStandingBidState
          :--> PBool
      )
pvalidateNewBid = phoistAcyclic $
  plam $ \auctionCs auctionTerms oldBidState newBidState ->
    pmaybeData
      # pcon PFalse -- The new bid state should not be empty.
      # plam
        ( \newTerms ->
            (pvalidateBidTerms # auctionCs # auctionTerms # newTerms)
              #&& (pvalidateCompareBids # auctionTerms # oldBidState # newTerms)
        )
      # pto newBidState

pvalidateCompareBids :: Term s (PAuctionTerms :--> PStandingBidState :--> PBidTerms :--> PBool)
pvalidateCompareBids = phoistAcyclic $
  plam $ \auctionTerms oldBidState newTerms ->
    pmaybeData
      # (pvalidateStartingBid # auctionTerms # newTerms)
      # plam (\oldTerms -> pvalidateBidIncrement # auctionTerms # oldTerms # newTerms)
      # pto oldBidState

-- The first bid's price is no smaller than the auction's starting price.
pvalidateStartingBid :: Term s (PAuctionTerms :--> PBidTerms :--> PBool)
pvalidateStartingBid = phoistAcyclic $
  plam $ \auctionTerms newTerms -> P.do
    startingBid <- plet $ pfromData $ pfield @"startingBid" # auctionTerms
    bidPrice <- plet $ pfromData $ pfield @"btPrice" # newTerms
    startingBid #<= bidPrice

-- The difference between the old and new bid price is no smaller than
-- the auction's minimum bid increment.
pvalidateBidIncrement :: Term s (PAuctionTerms :--> PBidTerms :--> PBidTerms :--> PBool)
pvalidateBidIncrement = phoistAcyclic $
  plam $ \auctionTerms oldTerms newTerms -> P.do
    oldBidPrice <- plet $ pfromData $ pfield @"btPrice" # oldTerms
    newBidPrice <- plet $ pfromData $ pfield @"btPrice" # newTerms
    minBidIncrement <- plet $ pfromData $ pfield @"minBidIncrement" # auctionTerms
    oldBidPrice + minBidIncrement #<= newBidPrice
