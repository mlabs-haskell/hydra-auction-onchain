module HydraAuctionOnchain.Lib.Cose
  ( pmkSigStructure
  ) where

import HydraAuctionOnchain.Lib.Serialization (pserializeAddress)
import Plutarch.Api.V2 (PAddress)
import Plutarch.Extra.Maybe (pjust, pnothing)

pmkSigStructure :: Term s (PAddress :--> PByteString :--> PByteString :--> PMaybe PByteString)
pmkSigStructure = phoistAcyclic $
  plam $ \addr payload payloadLength -> P.do
    pmatch (pserializeAddress # addr) $ \case
      PNothing -> pnothing
      PJust addrBytes ->
        pjust
          # mconcat
            [ phexByteStr "846a5369676e61747572653158"
            , phexByteStr "46" -- FIXME
            , phexByteStr "A201276761646472657373"
            , addrBytes
            , phexByteStr "4058"
            , payloadLength
            , payload
            ]
