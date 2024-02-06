module HydraAuctionOnchain.Types.AuctionInfo
  ( PAuctionInfo (PAuctionInfo)
  ) where

import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import Plutarch.Api.V2 (PAddress, PCurrencySymbol)
import Plutarch.DataRepr (PDataFields)

newtype PAuctionInfo (s :: S)
  = PAuctionInfo
      ( Term
          s
          ( PDataRecord
              '[ "auctionId" ':= PCurrencySymbol
               , "auctionTerms" ':= PAuctionTerms
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
