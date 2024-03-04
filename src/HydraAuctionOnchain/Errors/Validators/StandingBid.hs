module HydraAuctionOnchain.Errors.Validators.StandingBid
  ( PStandingBidError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PStandingBidError (s :: S)
  = -- Common errors
    StandingBid'Error'MissingStandingBidInput
  | StandingBid'Error'TooManyOwnScriptInputs
  | StandingBid'Error'OwnInputMissingToken
  | StandingBid'Error'UnexpectedTokensMintedBurned
  | -- NewBid errors
    StandingBid'NewBid'Error'MissingOwnOutput
  | StandingBid'NewBid'Error'OwnOutputMissingToken
  | StandingBid'NewBid'Error'FailedToDecodeNewBid
  | StandingBid'NewBid'Error'InvalidNewBidState
  | StandingBid'NewBid'Error'IncorrectValidityInterval
  | -- MoveToHydra errors
    StandingBid'MoveToHydra'Error'MissingDelegateSignature
  | StandingBid'MoveToHydra'Error'IncorrectValidityInterval
  | -- ConcludeAuction errors
    StandingBid'ConcludeAuction'Error'MissingAuctionEscrowInput
  | StandingBid'ConcludeAuction'Error'InvalidAuctionEscrowRedeemer
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PStandingBidError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PStandingBidError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PStandingBidError s) where
  errorCodePrefix = "STBD"
