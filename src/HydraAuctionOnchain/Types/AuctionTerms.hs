module HydraAuctionOnchain.Types.AuctionTerms
  ( PAuctionTerms (PAuctionTerms)
  ) where

import Plutarch.Api.V2
  ( AmountGuarantees (Positive)
  , KeyGuarantees (Sorted)
  , PPOSIXTime
  , PPubKeyHash
  , PValue
  )
import Plutarch.DataRepr (PDataFields)

newtype PAuctionTerms (s :: S)
  = PAuctionTerms
      ( Term
          s
          ( PDataRecord
              '[ "auctionLot" ':= PValue 'Sorted 'Positive
               , "sellerPkh" ':= PPubKeyHash
               , "sellerVk" ':= PByteString
               , "delegates" ':= PBuiltinList (PAsData PPubKeyHash)
               , "biddingStart" ':= PPOSIXTime
               , "biddingEnd" ':= PPOSIXTime
               , "purchaseDeadline" ':= PPOSIXTime
               , "cleanup" ':= PPOSIXTime
               , "auctionFeePerDelegate" ':= PInteger
               , "startingBid" ':= PInteger
               , "minBidIncrement" ':= PInteger
               , "minDepositAmount" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PAuctionTerms where
  type DPTStrat _ = PlutusTypeData
