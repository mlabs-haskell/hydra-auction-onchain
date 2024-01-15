module HydraAuctionOnchain.Types.BidderInfo
  ( PBidderInfo (PBidderInfo)
  ) where

import Plutarch.Api.V2 (PAddress)
import Plutarch.DataRepr (PDataFields)

data PBidderInfo (s :: S)
  = PBidderInfo
      ( Term
          s
          ( PDataRecord
              '[ "biBidderAddress" ':= PAddress
               , "biBidderVk" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PBidderInfo where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PBidderInfo
