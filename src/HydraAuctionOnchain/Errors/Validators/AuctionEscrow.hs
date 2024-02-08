module HydraAuctionOnchain.Errors.Validators.AuctionEscrow
  ( PAuctionEscrowError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PAuctionEscrowError (s :: S)
  = -- Common errors
    AuctionEscrow'Error'InvalidSellerAddress
  | AuctionEscrow'Error'MissingAuctionEscrowInput
  | AuctionEscrow'Error'TooManyOwnScriptInputs
  | AuctionEscrow'Error'OwnInputMissingToken
  | -- StartBidding errors
    AuctionEscrow'StartBidding'Error'UnexpectedTokensMintedBurned
  | AuctionEscrow'StartBidding'Error'IncorrectValidityInterval
  | AuctionEscrow'StartBidding'Error'MissingSellerSignature
  | AuctionEscrow'StartBidding'Error'MissingAuctionEscrowOutput
  | AuctionEscrow'StartBidding'Error'AuctionEscrowOutputMissingToken
  | AuctionEscrow'StartBidding'Error'FailedToDecodeAuctionEscrowState
  | AuctionEscrow'StartBidding'Error'InvalidAuctionStateTransition
  | AuctionEscrow'StartBidding'Error'MissingStandingBidOutput
  | AuctionEscrow'StartBidding'Error'FailedToDecodeStandingBidState
  | AuctionEscrow'StartBidding'Error'InvalidStandingBidState
  | -- BidderBuys errors
    AuctionEscrow'BidderBuys'Error'UnexpectedTokensMintedBurned
  | AuctionEscrow'BidderBuys'Error'IncorrectValidityInterval
  | AuctionEscrow'BidderBuys'Error'MissingAuctionEscrowOutput
  | AuctionEscrow'BidderBuys'Error'AuctionEscrowOutputMissingAuctionEscrowToken
  | AuctionEscrow'BidderBuys'Error'AuctionEscrowOutputMissingStandingBidToken
  | AuctionEscrow'BidderBuys'Error'FailedToDecodeAuctionEscrowState
  | AuctionEscrow'BidderBuys'Error'InvalidAuctionStateTransition
  | AuctionEscrow'BidderBuys'Error'MissingStandingBidInput
  | AuctionEscrow'BidderBuys'Error'StandingBidInputMissingToken
  | AuctionEscrow'BidderBuys'Error'FailedToDecodeStandingBidState
  | AuctionEscrow'BidderBuys'Error'EmptyStandingBid
  | AuctionEscrow'BidderBuys'Error'InvalidBidderAddress
  | AuctionEscrow'BidderBuys'Error'BidTermsInvalid
  | AuctionEscrow'BidderBuys'Error'AuctionLotNotPaidToBidder
  | AuctionEscrow'BidderBuys'Error'NoBidderConsent
  | AuctionEscrow'BidderBuys'Error'SellerPaymentIncorrect
  | AuctionEscrow'BidderBuys'Error'PaymentToFeeEscrowIncorrect
  | -- SellerReclaims errors
    AuctionEscrow'SellerReclaims'Error'UnexpectedTokensMintedBurned
  | AuctionEscrow'SellerReclaims'Error'IncorrectValidityInterval
  | AuctionEscrow'SellerReclaims'Error'MissingAuctionEscrowOutput
  | AuctionEscrow'SellerReclaims'Error'AuctionEscrowOutputMissingAuctionEscrowToken
  | AuctionEscrow'SellerReclaims'Error'AuctionEscrowOutputMissingStandingBidToken
  | AuctionEscrow'SellerReclaims'Error'FailedToDecodeAuctionEscrowState
  | AuctionEscrow'SellerReclaims'Error'InvalidAuctionStateTransition
  | AuctionEscrow'SellerReclaims'Error'PaymentToSellerIncorrect
  | AuctionEscrow'SellerReclaims'Error'NoSellerConsent
  | AuctionEscrow'SellerReclaims'Error'PaymentToFeeEscrowIncorrect
  | -- CleanupAuction errors
    AuctionEscrow'CleanupAuction'Error'AuctionTokensNotBurnedExactly
  | AuctionEscrow'CleanupAuction'Error'IncorrectValidityInterval
  | AuctionEscrow'CleanupAuction'Error'NoSellerConsent
  | AuctionEscrow'CleanupAuction'Error'AuctionIsNotConcluded
  | AuctionEscrow'CleanupAuction'Error'AuctionEscrowInputMissingStandingBidToken
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PAuctionEscrowError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PAuctionEscrowError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PAuctionEscrowError s) where
  errorCodePrefix = "AUES"
