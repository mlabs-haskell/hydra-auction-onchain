module HydraAuctionOnchain.Types.BidTerms
  ( PBidTerms (PBidTerms)
  ) where

import HydraAuctionOnchain.Types.BidderInfo (PBidderInfo)
import Plutarch.DataRepr (PDataFields)

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
