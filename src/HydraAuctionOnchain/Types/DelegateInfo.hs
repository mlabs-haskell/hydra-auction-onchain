module HydraAuctionOnchain.Types.DelegateInfo
  ( PDelegateInfo (PDelegateInfo)
  ) where

import Plutarch.DataRepr (PDataFields)

newtype PDelegateInfo (s :: S)
  = PDelegateInfo
      ( Term
          s
          ( PDataRecord
              '[ "httpServers" ':= PBuiltinList (PAsData PByteString)
               , "wsServers" ':= PBuiltinList (PAsData PByteString)
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PDelegateInfo where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PDelegateInfo
