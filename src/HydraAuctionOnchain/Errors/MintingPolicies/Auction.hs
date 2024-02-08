module HydraAuctionOnchain.Errors.MintingPolicies.Auction
  ( PAuctionMpError (..)
  ) where

import Data.Universe (Universe (universe), universeGeneric)
import HydraAuctionOnchain.Types.Error (ErrorCodePrefix (errorCodePrefix))

data PAuctionMpError (s :: S)
  = -- Common errors
    AuctionMp'Error'MissingOwnCurrencySymbol
  | -- MintAuction errors
    AuctionMp'MintAuction'Error'AuctionTokenBundleNotMinted
  | AuctionMp'MintAuction'Error'MissingUtxoNonceInput
  | AuctionMp'MintAuction'Error'MissingAuctionMetadataOutput
  | AuctionMp'MintAuction'Error'AuctionMetadataOutputMissingToken
  | AuctionMp'MintAuction'Error'FailedToDecodeAuctionInfo
  | AuctionMp'MintAuction'Error'AuctionInfoCurrencySymbolMismatch
  | -- BurnAuction errors
    AuctionMp'BurnAuction'Error'AuctionTokenBundleNotBurned
  deriving stock (Generic, Eq)
  deriving anyclass (PlutusType)

instance DerivePlutusType PAuctionMpError where
  type DPTStrat _ = PlutusTypeScott

instance Universe (PAuctionMpError s) where
  universe = universeGeneric

instance ErrorCodePrefix (PAuctionMpError s) where
  errorCodePrefix = "AUMP"
