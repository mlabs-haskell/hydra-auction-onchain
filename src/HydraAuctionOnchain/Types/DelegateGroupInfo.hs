module HydraAuctionOnchain.Types.DelegateGroupInfo
  ( PDelegateGroupInfo (PDelegateGroupInfo)
  ) where

import HydraAuctionOnchain.Types.DelegateInfo (PDelegateInfo)
import Plutarch.Api.V2 (PCurrencySymbol, PPubKeyHash)
import Plutarch.DataRepr (PDataFields)

newtype PDelegateGroupInfo (s :: S)
  = PDelegateGroupInfo
      ( Term
          s
          ( PDataRecord
              '[ "delegateGroupId" ':= PCurrencySymbol
               , "delegateGroupMasterKeys" ':= PBuiltinList (PAsData PPubKeyHash)
               , "delegateGroupServers" ':= PDelegateInfo
               , "delegateGroupMetadata" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PDelegateGroupInfo where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PDelegateGroupInfo
