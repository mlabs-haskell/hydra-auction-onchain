module HydraAuctionOnchain.Lib.Cose
  ( pmkSigStructure
  ) where

import HydraAuctionOnchain.Lib.Serialization (pserializeAddress)
import Plutarch.Api.V2 (PAddress)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Monadic qualified as P

pmkSigStructure :: Term s (PAddress :--> PByteString :--> PByteString :--> PMaybe PByteString)
pmkSigStructure = phoistAcyclic $
  plam $ \addr payload payloadLength -> P.do
    pmatch (pserializeAddress # addr) $ \case
      PNothing -> pnothing
      PJust addrSerialized -> P.do
        addrSerializedFields <- pletFields @["addrCbor", "addrMapEntrySize"] addrSerialized
        pjust
          # mconcat
            [ phexByteStr "846a5369676e61747572653158"
            , addrSerializedFields.addrMapEntrySize
            , phexByteStr "A201276761646472657373" -- 61646472657373 = "address"
            , addrSerializedFields.addrCbor
            , phexByteStr "4058"
            , payloadLength
            , payload
            ]
