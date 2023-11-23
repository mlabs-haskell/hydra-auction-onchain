module HydraAuctionOnchain.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , standingBidTokenName
  ) where

import Plutarch.Api.V2 (PTokenName)

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: Term s PTokenName
auctionEscrowTokenName = pconstant "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: Term s PTokenName
auctionMetadataTokenName = pconstant "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: Term s PTokenName
standingBidTokenName = pconstant "STANDING_BID"
