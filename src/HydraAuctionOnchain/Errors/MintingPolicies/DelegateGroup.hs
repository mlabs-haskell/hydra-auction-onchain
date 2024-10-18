module HydraAuctionOnchain.Errors.MintingPolicies.DelegateGroup
  ( PDelegateGroupMpError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PDelegateGroupMpError (s :: S)
  = -- Common errors
    DelegateGroupMp'Error'MissingOwnCurrencySymbol
  | -- MintDelegateGroup errors
    DelegateGroupMp'Mint'Error'DelegateGroupTokenNotMinted
  | DelegateGroupMp'Mint'Error'MissingUtxoNonceInput
  | DelegateGroupMp'Mint'Error'MissingMetadataOutput
  | DelegateGroupMp'Mint'Error'MetadataOutputMissingToken
  | DelegateGroupMp'Mint'Error'FailedToDecodeDelegateGroupInfo
  | DelegateGroupMp'Mint'Error'DelegateGroupCurrencySymbolMismatch
  | DelegateGroupMp'Mint'Error'MissingDelegateSignatures
  | -- BurnDelegateGroup errors
    DelegateGroupMp'Burn'Error'DelegateGroupTokenNotBurned
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PDelegateGroupMpError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PDelegateGroupMpError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PDelegateGroupMpError s) where
  errorCodePrefix = "DGMP"
