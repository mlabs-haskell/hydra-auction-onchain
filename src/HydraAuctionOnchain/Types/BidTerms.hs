module HydraAuctionOnchain.Types.BidTerms
  ( PBidTerms (PBidTerms)
  , psellerPayout
  , pvalidateBidTerms
  ) where

import HydraAuctionOnchain.Helpers (pserialise)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, ptotalAuctionFees)
import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import Plutarch.Api.V2 (PCurrencySymbol, PPubKeyHash)
import Plutarch.Crypto (pverifyEd25519Signature)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Monadic qualified as P

data PBidTerms (s :: S)
  = PBidTerms
      ( Term
          s
          ( PDataRecord
              '[ "btBidder" ':= PBidderInfo
               , "btPrice" ':= PInteger
               , "btBidderSignature" ':= PByteString
               , "btSellerSignature" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PBidTerms where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PBidTerms

psellerPayout :: Term s (PAuctionTerms :--> PBidTerms :--> PInteger)
psellerPayout = phoistAcyclic $
  plam $ \auctionTerms bidTerms -> P.do
    bidPrice <- plet $ pfield @"btPrice" # bidTerms
    totalAuctionFees <- plet $ ptotalAuctionFees # auctionTerms
    bidPrice - totalAuctionFees

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

pvalidateBidTerms :: Term s (PCurrencySymbol :--> PAuctionTerms :--> PBidTerms :--> PBool)
pvalidateBidTerms = phoistAcyclic $
  plam $ \auctionCs auctionTerms bidTerms -> P.do
    bidTermsFields <-
      pletFields
        @["btBidder", "btPrice", "btBidderSignature", "btSellerSignature"]
        bidTerms
    bidderInfo <- pletFields @["biBidderPkh", "biBidderVk"] bidTermsFields.btBidder

    let sellerSignature = bidTermsFields.btSellerSignature
    sellerVk <- plet $ pfield @"sellerVk" # auctionTerms
    sellerSignatureMsg <-
      plet $
        sellerSignatureMessage
          # auctionCs
          # bidderInfo.biBidderVk

    let
      bidderSignature = bidTermsFields.btBidderSignature
      bidderVk = bidderInfo.biBidderVk
    bidderSignatureMsg <-
      plet $
        bidderSignatureMessage
          # auctionCs
          # bidTermsFields.btPrice
          # bidderInfo.biBidderPkh

    -- The seller authorized the bidder to participate in the auction.
    (pverifyEd25519Signature # sellerVk # sellerSignatureMsg # sellerSignature)
      -- The bidder authorized the bid to be submitted in the auction.
      #&& (pverifyEd25519Signature # bidderVk # bidderSignatureMsg # bidderSignature)

bidderSignatureMessage
  :: Term
      s
      ( PCurrencySymbol
          :--> PInteger
          :--> PPubKeyHash
          :--> PByteString
      )
bidderSignatureMessage = phoistAcyclic $
  plam $ \auctionCs bidPrice bidderPkh ->
    (pserialise # auctionCs) <> (pserialise # bidderPkh) <> (pserialise # bidPrice)

sellerSignatureMessage :: Term s (PCurrencySymbol :--> PByteString :--> PByteString)
sellerSignatureMessage = phoistAcyclic $
  plam $ \auctionCs bidderVk ->
    (pserialise # auctionCs) <> (pserialise # bidderVk)
