{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Types.AuctionTerms
  ( PAuctionTerms (PAuctionTerms)
  , pbiddingPeriod
  , pcleanupPeriod
  , ppostBiddingPeriod
  , ppostPurchasePeriod
  , ppurchasePeriod
  , ptotalAuctionFees
  , pvalidateAuctionTerms
  ) where

import HydraAuctionOnchain.Errors.Types.AuctionTerms (PAuctionTermsError (..))
import HydraAuctionOnchain.Helpers (pintervalFiniteClosedOpen)
import HydraAuctionOnchain.Lib.Address (paddrPaymentKeyHash)
import HydraAuctionOnchain.Lib.Value (pvaluePositive)
import HydraAuctionOnchain.Types.Error (errCode, passert)
import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PAddress
  , PPOSIXTime
  , PPOSIXTimeRange
  , PPubKeyHash
  , PValue
  )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.Field (pletAll)
import Plutarch.Extra.Interval qualified as Interval (pfrom)
import Plutarch.Extra.Maybe (pisJust)
import Plutarch.Extra.Value (padaOf)
import Plutarch.Monadic qualified as P
import Ply.Plutarch (PlyArgOf)

newtype PAuctionTerms (s :: S)
  = PAuctionTerms
      ( Term
          s
          ( PDataRecord
              '[ "auctionLot" ':= PValue 'Sorted 'Positive
               , "sellerAddress" ':= PAddress
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

instance PTryFrom PData PAuctionTerms

data AuctionTerms

type instance PlyArgOf PAuctionTerms = AuctionTerms

pvalidateAuctionTerms :: Term s (PAuctionTerms :--> PUnit)
pvalidateAuctionTerms = phoistAcyclic $
  plam $ \auctionTerms -> P.do
    rec <- pletAll auctionTerms

    -- Auction lot must not include any ADA.
    passert $(errCode AuctionTerms'Error'AuctionLotNonZeroAda) $
      padaOf # rec.auctionLot #== 0

    -- All amounts in the auction lot must be positive.
    passert $(errCode AuctionTerms'Error'NonPositiveAuctionLotValue) $
      pvaluePositive # rec.auctionLot

    -- The payment part of the seller address must be seller pkh.
    passert $(errCode AuctionTerms'Error'SellerAddressLacksPubKeyCredential) $
      pisJust #$ paddrPaymentKeyHash # rec.sellerAddress

    -- TODO: The seller pubkey hash corresponds to the seller verification key.
    -- Note: this check only becomes possible on-chain in Plutus V3.
    -- https://github.com/input-output-hk/plutus/pull/5431

    -- Bidding ends after it the bidding start time.
    passert $(errCode AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd) $
      pfromData rec.biddingStart #< pfromData rec.biddingEnd

    -- The purchase deadline occurs after bidding ends.
    passert $(errCode AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline) $
      pfromData rec.biddingEnd #< pfromData rec.purchaseDeadline

    -- Cleanup happens after the purchase deadline,
    -- so that the seller can claim the winning bidder's deposit
    -- if the auction lot is not sold.
    passert $(errCode AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup) $
      pfromData rec.purchaseDeadline #< pfromData rec.cleanup

    -- New bids must be larger than the standing bid.
    passert $(errCode AuctionTerms'Error'NonPositiveMinBidIncrement) $
      0 #< pfromData rec.minBidIncrement

    -- The auction fees for all delegates must be covered by
    -- the starting bid.
    passert $(errCode AuctionTerms'Error'InvalidStartingBid) $
      ptotalAuctionFees # auctionTerms #< rec.startingBid

    -- The auction fee for each delegate must contain
    -- the min 2.5 ADA for the utxos that will be sent to the delegates
    -- during fee distribution.
    passert $(errCode AuctionTerms'Error'InvalidAuctionFeePerDelegate) $
      pminAuctionFee #< rec.auctionFeePerDelegate

    -- There must be at least one delegate.
    passert $(errCode AuctionTerms'Error'NoDelegates) $
      0 #< plength # pfromData rec.delegates

    pcon PUnit

ptotalAuctionFees :: Term s (PAuctionTerms :--> PInteger)
ptotalAuctionFees = phoistAcyclic $
  plam $ \auctionTerms -> P.do
    auctionTermsFields <- pletFields @["delegates", "auctionFeePerDelegate"] auctionTerms
    (plength # pfromData auctionTermsFields.delegates)
      * auctionTermsFields.auctionFeePerDelegate

pminAuctionFee :: Term s PInteger
pminAuctionFee = pconstant 2_500_000

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

pcleanupPeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
pcleanupPeriod = phoistAcyclic $
  plam $ \auctionTerms ->
    Interval.pfrom #$ pfield @"cleanup" # auctionTerms

ppostBiddingPeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
ppostBiddingPeriod = phoistAcyclic $
  plam $ \auctionTerms ->
    Interval.pfrom #$ pfield @"biddingEnd" # auctionTerms

ppostPurchasePeriod :: Term s (PAuctionTerms :--> PPOSIXTimeRange)
ppostPurchasePeriod = phoistAcyclic $
  plam $ \auctionTerms ->
    Interval.pfrom #$ pfield @"purchaseDeadline" # auctionTerms
