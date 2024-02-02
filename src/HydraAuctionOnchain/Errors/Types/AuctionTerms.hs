module HydraAuctionOnchain.Errors.Types.AuctionTerms
  ( PAuctionTermsError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PAuctionTermsError (s :: S)
  = AuctionTerms'Error'AuctionLotNonZeroAda
  | AuctionTerms'Error'NonPositiveAuctionLotValue
  | AuctionTerms'Error'SellerAddressLacksPubKeyCredential
  | AuctionTerms'Error'SellerVkPkhMismatch
  | AuctionTerms'Error'BiddingStartNotBeforeBiddingEnd
  | AuctionTerms'Error'BiddingEndNotBeforePurchaseDeadline
  | AuctionTerms'Error'PurchaseDeadlineNotBeforeCleanup
  | AuctionTerms'Error'NonPositiveMinBidIncrement
  | AuctionTerms'Error'InvalidStartingBid
  | AuctionTerms'Error'InvalidAuctionFeePerDelegate
  | AuctionTerms'Error'NoDelegates
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PAuctionTermsError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PAuctionTermsError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PAuctionTermsError s) where
  errorCodePrefix = "AUTE"
