module HydraAuctionOnchain.Types.AuctionTerms
  ( PAuctionTerms (PAuctionTerms)
  , pbiddingPeriod
  , pcleanupPeriod
  , ppenaltyPeriod
  , ppurchasePeriod
  , ptotalAuctionFees
  ) where

import HydraAuctionOnchain.Helpers (pintervalFiniteClosedOpen)
import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PPOSIXTime
  , PPOSIXTimeRange
  , PPubKeyHash
  , PValue
  )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.Interval qualified as Interval (pfrom)
import Plutarch.Monadic qualified as P
import Ply.Plutarch (PlyArgOf)

newtype PAuctionTerms (s :: S)
  = PAuctionTerms
      ( Term
          s
          ( PDataRecord
              '[ "auctionLot" ':= PValue 'Sorted 'Positive
               , "sellerPkh" ':= PPubKeyHash
               , "sellerVk" ':= PByteString
               , "delegates" ':= PBuiltinList (PAsData PPubKeyHash)
               , "biddingStart" ':= PPOSIXTime
               , "biddingEnd" ':= PPOSIXTime
               , "purchaseDeadline" ':= PPOSIXTime
               , "cleanup" ':= PPOSIXTime
               , "auctionFeePerDelegate" ':= PInteger
               , "startingBid" ':= PInteger
               , "minBidIncrement" ':= PInteger
               , "minDepositAmount" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PAuctionTerms where
  type DPTStrat _ = PlutusTypeData

data AuctionTerms

type instance PlyArgOf PAuctionTerms = AuctionTerms

ptotalAuctionFees :: Term s (PAuctionTerms :--> PInteger)
ptotalAuctionFees = phoistAcyclic $
  plam $ \auctionTerms -> P.do
    auctionTermsFields <- pletFields @["delegates", "auctionFeePerDelegate"] auctionTerms
    (plength # pfromData auctionTermsFields.delegates)
      * auctionTermsFields.auctionFeePerDelegate

--------------------------------------------------------------------------------
-- Auction Lifecycle
--------------------------------------------------------------------------------

pbiddingPeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
pbiddingPeriod = phoistAcyclic $
  plam $ \auctionTerms -> P.do
    auctionTermsFields <- pletFields @["biddingStart", "biddingEnd"] auctionTerms
    pintervalFiniteClosedOpen
      # auctionTermsFields.biddingStart
      # auctionTermsFields.biddingEnd

ppurchasePeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
ppurchasePeriod = phoistAcyclic $
  plam $ \auctionTerms -> P.do
    auctionTermsFields <- pletFields @["biddingEnd", "purchaseDeadline"] auctionTerms
    pintervalFiniteClosedOpen
      # auctionTermsFields.biddingEnd
      # auctionTermsFields.purchaseDeadline

ppenaltyPeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
ppenaltyPeriod = phoistAcyclic $
  plam $ \auctionTerms -> P.do
    auctionTermsFields <- pletFields @["purchaseDeadline", "cleanup"] auctionTerms
    pintervalFiniteClosedOpen
      # auctionTermsFields.purchaseDeadline
      # auctionTermsFields.cleanup

pcleanupPeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
pcleanupPeriod = phoistAcyclic $
  plam $ \auctionTerms ->
    Interval.pfrom #$ pfield @"cleanup" # auctionTerms
