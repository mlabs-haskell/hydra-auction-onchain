module HydraAuctionOnchain.Types.AuctionInfo
  ( PAuctionInfo (PAuctionInfo)
  ) where

import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import HydraAuctionOnchain.Types.DelegateInfo (PDelegateInfo)
import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PMaybeData)
import Plutarch.DataRepr (PDataFields)

newtype PAuctionInfo (s :: S)
  = PAuctionInfo
      ( Term
          s
          ( PDataRecord
              '[ "auctionId" ':= PCurrencySymbol
               , "auctionTerms" ':= PAuctionTerms
               , "delegateInfo" ':= PMaybeData PDelegateInfo
               , "auctionEscrowAddr" ':= PAddress
               , "bidderDepositAddr" ':= PAddress
               , "feeEscrowAddr" ':= PAddress
               , "standingBidAddr" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PAuctionInfo where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PAuctionInfo
