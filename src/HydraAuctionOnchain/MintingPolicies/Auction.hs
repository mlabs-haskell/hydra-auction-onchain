module HydraAuctionOnchain.MintingPolicies.Auction
  ( allAuctionTokensBurned
  , auctionEscrowTokenName
  , auctionMetadataTokenName
  , standingBidTokenName
  ) where

import Plutarch.Api.V1.Value qualified as Value (psingleton)
import Plutarch.Api.V2
  ( AmountGuarantees (NonZero)
  , KeyGuarantees (Sorted)
  , PCurrencySymbol
  , PTokenName
  , PValue
  )
import Plutarch.Monadic qualified as P

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: Term s PTokenName
auctionEscrowTokenName = pconstant "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: Term s PTokenName
auctionMetadataTokenName = pconstant "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: Term s PTokenName
standingBidTokenName = pconstant "STANDING_BID"

allAuctionTokensBurned :: Term s (PCurrencySymbol :--> PValue 'Sorted 'NonZero)
allAuctionTokensBurned = phoistAcyclic $
  plam $ \auctionCs -> P.do
    mkValue <- plet $ plam $ \tn -> Value.psingleton # auctionCs # tn # (-1)
    (mkValue # auctionEscrowTokenName)
      <> (mkValue # auctionMetadataTokenName)
      <> (mkValue # standingBidTokenName)
