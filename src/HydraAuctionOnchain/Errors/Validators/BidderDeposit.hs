module HydraAuctionOnchain.Errors.Validators.BidderDeposit
  ( PBidderDepositError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PBidderDepositError (s :: S)
  = -- Common errors
    BidderDeposit'Error'MissingOwnInput
  | BidderDeposit'Error'TooManyOwnScriptInputs
  | BidderDeposit'Error'UnexpectedTokensMintedBurned
  | -- UseDepositWinner errors
    BidderDeposit'UseDepositWinner'Error'MissingStandingBidInput
  | BidderDeposit'UseDepositWinner'Error'StandingBidInputMissingToken
  | BidderDeposit'UseDepositWinner'Error'FailedToDecodeStandingBidState
  | BidderDeposit'UseDepositWinner'Error'BidderNotWinner
  | BidderDeposit'UseDepositWinner'Error'MissingAuctionEscrowInput
  | BidderDeposit'UseDepositWinner'Error'AuctionEscrowInputMissingToken
  | BidderDeposit'UseDepositWinner'Error'InvalidAuctionEscrowRedeemer
  | -- ReclaimDepositLoser errors
    BidderDeposit'ReclaimDepositLoser'Error'MissingStandingBidInput
  | BidderDeposit'ReclaimDepositLoser'Error'StandingBidInputMissingToken
  | BidderDeposit'ReclaimDepositLoser'Error'FailedToDecodeStandingBidState
  | BidderDeposit'ReclaimDepositLoser'Error'BidderNotLoser
  | BidderDeposit'ReclaimDepositLoser'Error'IncorrectValidityInterval
  | BidderDeposit'ReclaimDepositLoser'Error'InvalidBidderAddress
  | BidderDeposit'ReclaimDepositLoser'Error'NoBidderConsent
  | -- ReclaimDepositAuctionConcluded errors
    BidderDeposit'ReclaimDepositConcluded'Error'MissingAuctionRefInput
  | BidderDeposit'ReclaimDepositConcluded'Error'AuctionRefInputMissingToken
  | BidderDeposit'ReclaimDepositConcluded'Error'FailedToDecodeAuctionState
  | BidderDeposit'ReclaimDepositConcluded'Error'AuctionNotConcluded
  | BidderDeposit'ReclaimDepositConcluded'Error'IncorrectValidityInterval
  | BidderDeposit'ReclaimDepositConcluded'Error'InvalidBidderAddress
  | BidderDeposit'ReclaimDepositConcluded'Error'NoBidderConsent
  | -- ReclaimDepositCleanup errors
    BidderDeposit'ReclaimDepositCleanup'Error'IncorrectValidityInterval
  | BidderDeposit'ReclaimDepositCleanup'Error'InvalidBidderAddress
  | BidderDeposit'ReclaimDepositCleanup'Error'NoBidderConsent
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PBidderDepositError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PBidderDepositError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PBidderDepositError s) where
  errorCodePrefix = "BIDE"
