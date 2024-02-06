module HydraAuctionOnchain.Types.Tokens
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , pauctionTokenBundleBurned
  , pauctionTokenBundleMinted
  , ptxOutContainsAuctionEscrowToken
  , ptxOutContainsAuctionMetadataToken
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

pauctionTokenBundle :: Term s (PCurrencySymbol :--> PInteger :--> PValue 'Sorted 'NonZero)
pauctionTokenBundle = phoistAcyclic $
  plam $ \auctionCs amount -> P.do
    mkValue <- plet $ plam $ \tn -> Value.psingleton # auctionCs # tn # amount
    (mkValue # auctionMetadataTokenName)
      <> (mkValue # auctionEscrowTokenName)
      <> (mkValue # standingBidTokenName)

pauctionTokenBundleMinted :: Term s (PCurrencySymbol :--> PValue 'Sorted 'NonZero)
pauctionTokenBundleMinted = phoistAcyclic $
  plam $ \auctionCs ->
    pauctionTokenBundle # auctionCs # 1

pauctionTokenBundleBurned :: Term s (PCurrencySymbol :--> PValue 'Sorted 'NonZero)
pauctionTokenBundleBurned = phoistAcyclic $
  plam $ \auctionCs ->
    pauctionTokenBundle # auctionCs # (-1)

ptxOutContainsAuctionToken :: Term s (PCurrencySymbol :--> PTokenName :--> PTxOut :--> PBool)
ptxOutContainsAuctionToken = phoistAcyclic $
  plam $ \auctionCs tn txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # tn) #== 1

ptxOutContainsAuctionMetadataToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsAuctionMetadataToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    ptxOutContainsAuctionToken # auctionCs # auctionMetadataTokenName # txOut

ptxOutContainsAuctionEscrowToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsAuctionEscrowToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    ptxOutContainsAuctionToken # auctionCs # auctionEscrowTokenName # txOut

ptxOutContainsStandingBidToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsStandingBidToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    ptxOutContainsAuctionToken # auctionCs # standingBidTokenName # txOut
