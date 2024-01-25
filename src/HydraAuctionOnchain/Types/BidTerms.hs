{-# LANGUAGE PackageImports #-}

module HydraAuctionOnchain.Types.BidTerms
  ( PBidTerms (PBidTerms)
  , psellerPayout
  , pvalidateBidTerms
  ) where

import HydraAuctionOnchain.Helpers (pserialise)
import HydraAuctionOnchain.Lib.Address (paddrPaymentKeyHashUnsafe)
import HydraAuctionOnchain.Lib.Cose (pverifyCoseSignature)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, ptotalAuctionFees)
import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import Plutarch.Api.V2 (PCurrencySymbol, PPubKeyHash)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Monadic qualified as P
import "liqwid-plutarch-extra" Plutarch.Extra.List (preplicate)

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
    sellerAddr <- plet $ pfield @"sellerAddress" # auctionTerms
    bidTermsFields <-
      pletFields
        @["btBidder", "btPrice", "btBidderSignature", "btSellerSignature"]
        bidTerms
    bidderInfo <- pletFields @["biBidderAddress", "biBidderVk"] bidTermsFields.btBidder

    let sellerSignature = bidTermsFields.btSellerSignature
    sellerVk <- plet $ pfield @"sellerVk" # auctionTerms
    sellerSigMessage <-
      plet $
        sellerSignatureMessage
          # auctionCs
          # bidderInfo.biBidderVk

    let
      bidderSignature = bidTermsFields.btBidderSignature
      bidderVk = bidderInfo.biBidderVk
      bidderAddr = bidderInfo.biBidderAddress
    bidderSigMessage <-
      plet $
        bidderSignatureMessage
          # auctionCs
          # bidTermsFields.btPrice
          # (paddrPaymentKeyHashUnsafe # bidderAddr)

    -- The seller authorized the bidder to participate in the auction.
    ( pverifyCoseSignature
        # sellerSignature
        # sellerVk
        # sellerAddr
        # sellerSigMessage
        # sellerSigMessageLengthHex
      )
      -- The bidder authorized the bid to be submitted in the auction.
      #&& ( pverifyCoseSignature
              # bidderSignature
              # bidderVk
              # bidderAddr
              # bidderSigMessage
              # bidderSigMessageLengthHex
          )

bidderSigMessageLengthHex :: Term s PByteString
bidderSigMessageLengthHex =
  -- 69 = 2 (cbor) + 28 (cs) + 2 (cbor) + 28 (pkh) + 9 (lovelace)
  phoistAcyclic $ phexByteStr "45"

sellerSigMessageLengthHex :: Term s PByteString
sellerSigMessageLengthHex =
  -- 64 = 2 (cbor) + 28 (cs) + 2 (cbor) + 32 (vk)
  phoistAcyclic $ phexByteStr "40"

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
    padMessage # 69 #$ (pserialise # auctionCs)
      <> (pserialise # bidderPkh)
      <> (pserialise # bidPrice)

sellerSignatureMessage :: Term s (PCurrencySymbol :--> PByteString :--> PByteString)
sellerSignatureMessage = phoistAcyclic $
  plam $ \auctionCs bidderVk ->
    (pserialise # auctionCs) <> (pserialise # bidderVk)

padMessage :: Term s (PInteger :--> PByteString :--> PByteString)
padMessage = phoistAcyclic $
  plam $ \targetSize message -> P.do
    padSize <- plet $ targetSize - (plengthBS # message)
    let nul = phexByteStr "00"
    let padding = pfoldl # plam (<>) # mempty #$ preplicate @PBuiltinList # padSize # nul
    pif (padSize #<= 0) message (padding <> message)
