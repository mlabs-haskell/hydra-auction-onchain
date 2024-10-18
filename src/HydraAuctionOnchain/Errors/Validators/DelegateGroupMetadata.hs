module HydraAuctionOnchain.Errors.Validators.DelegateGroupMetadata
  ( PDelegateGroupMetadataError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PDelegateGroupMetadataError (s :: S)
  = -- RemoveDelegateGroup errors
    DelegateGroupMetadata'Remove'Error'MetadataOutputMissingToken
  | DelegateGroupMetadata'Remove'Error'DelegateGroupTokenNotBurned
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PDelegateGroupMetadataError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PDelegateGroupMetadataError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PDelegateGroupMetadataError s) where
  errorCodePrefix = "DGMD"
