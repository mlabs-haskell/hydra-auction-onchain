module HydraAuctionOnchain.Types.Tokens
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , delegateGroupTokenName
  , pauctionTokenBundleBurned
  , pauctionTokenBundleMinted
  , pauctionTokenBundleValueBurned
  , pauctionTokenBundleValueMinted
  , ptxOutContainsAuctionEscrowToken
  , ptxOutContainsAuctionMetadataToken
  , ptxOutContainsDelegateGroupToken
  , ptxOutContainsStandingBidToken
  , standingBidTokenName
  ) where

import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as Map (pempty, pinsert, psingleton)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2
  ( AmountGuarantees (NonZero)
  , KeyGuarantees (Sorted)
  , PCurrencySymbol
  , PTokenName
  , PTxOut
  , PValue (PValue)
  )

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: Term s PTokenName
auctionEscrowTokenName = pconstant "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: Term s PTokenName
auctionMetadataTokenName = pconstant "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: Term s PTokenName
standingBidTokenName = pconstant "STANDING_BID"

-- | Delegate group token, identifying the true delegate group.
delegateGroupTokenName :: Term s PTokenName
delegateGroupTokenName = pconstant "DELEGATE_GROUP"

ptxOutContainsToken :: Term s (PCurrencySymbol :--> PTokenName :--> PTxOut :--> PBool)
ptxOutContainsToken = phoistAcyclic $
  plam $ \auctionCs tn txOut ->
    (pvalueOf # (pfield @"value" # txOut) # auctionCs # tn) #== 1

ptxOutContainsAuctionMetadataToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsAuctionMetadataToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    ptxOutContainsToken # auctionCs # auctionMetadataTokenName # txOut

ptxOutContainsAuctionEscrowToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsAuctionEscrowToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    ptxOutContainsToken # auctionCs # auctionEscrowTokenName # txOut

ptxOutContainsStandingBidToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsStandingBidToken = phoistAcyclic $
  plam $ \auctionCs txOut ->
    ptxOutContainsToken # auctionCs # standingBidTokenName # txOut

ptxOutContainsDelegateGroupToken :: Term s (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutContainsDelegateGroupToken = phoistAcyclic $
  plam $ \delegateGroupCs txOut ->
    ptxOutContainsToken # delegateGroupCs # delegateGroupTokenName # txOut

----------------------------------------------------------------------
-- Token bundle

pauctionTokenBundle :: Term s (PInteger :--> PMap 'Sorted PTokenName PInteger)
pauctionTokenBundle = phoistAcyclic $
  plam $ \amount ->
    Map.pinsert
      # auctionMetadataTokenName
      # amount
      #$ Map.pinsert
      # auctionEscrowTokenName
      # amount
      #$ Map.pinsert
      # standingBidTokenName
      # amount
      # Map.pempty

pauctionTokenBundleMinted :: Term s (PMap 'Sorted PTokenName PInteger)
pauctionTokenBundleMinted =
  phoistAcyclic $
    pauctionTokenBundle # 1

pauctionTokenBundleBurned :: Term s (PMap 'Sorted PTokenName PInteger)
pauctionTokenBundleBurned =
  phoistAcyclic $
    pauctionTokenBundle # (-1)

----------------------------------------------------------------------
-- Token bundle value

pauctionTokenBundleValue :: Term s (PCurrencySymbol :--> PInteger :--> PValue 'Sorted 'NonZero)
pauctionTokenBundleValue = phoistAcyclic $
  plam $ \auctionCs amount ->
    pcon $ PValue $ Map.psingleton # auctionCs #$ pauctionTokenBundle # amount

pauctionTokenBundleValueMinted :: Term s (PCurrencySymbol :--> PValue 'Sorted 'NonZero)
pauctionTokenBundleValueMinted = phoistAcyclic $
  plam $ \auctionCs ->
    pauctionTokenBundleValue # auctionCs # 1

pauctionTokenBundleValueBurned :: Term s (PCurrencySymbol :--> PValue 'Sorted 'NonZero)
pauctionTokenBundleValueBurned = phoistAcyclic $
  plam $ \auctionCs ->
    pauctionTokenBundleValue # auctionCs # (-1)
