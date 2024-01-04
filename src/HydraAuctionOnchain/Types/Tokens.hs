module HydraAuctionOnchain.Types.Tokens
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , pallAuctionTokensBurned
  , ptxOutContainsAuctionEscrowToken
  , ptxOutContainsStandingBidToken
  , standingBidTokenName
  ) where

import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V1.Value qualified as Value (psingleton)
import Plutarch.Api.V2
  ( AmountGuarantees (NonZero)
  , KeyGuarantees (Sorted)
  , PCurrencySymbol
  , PTokenName
  , PTxOut
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

pallAuctionTokensBurned :: Term s (PCurrencySymbol :--> PValue 'Sorted 'NonZero)
pallAuctionTokensBurned = phoistAcyclic $
  plam $ \auctionCs -> P.do
    mkValue <- plet $ plam $ \tn -> Value.psingleton # auctionCs # tn # (-1)
    (mkValue # auctionEscrowTokenName)
      <> (mkValue # auctionMetadataTokenName)
      <> (mkValue # standingBidTokenName)

ptxOutContainsAuctionEscrowToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsAuctionEscrowToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # auctionEscrowTokenName)
      #== 1

ptxOutContainsStandingBidToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsStandingBidToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # standingBidTokenName)
      #== 1
