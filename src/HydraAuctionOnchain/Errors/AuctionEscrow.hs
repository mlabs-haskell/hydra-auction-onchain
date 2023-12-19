module HydraAuctionOnchain.Errors.AuctionEscrow
  ( PAuctionEscrowError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PAuctionEscrowError (s :: S)
  = -- Common errors
    AuctionEscrow'Error'MissingAuctionEscrowInput
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
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PAuctionEscrowError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PAuctionEscrowError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PAuctionEscrowError s) where
  errorCodePrefix = "AUES"
