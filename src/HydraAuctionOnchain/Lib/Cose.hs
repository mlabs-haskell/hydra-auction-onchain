module HydraAuctionOnchain.Lib.Cose
  ( pmkSigStructure
  , pverifyCoseSignature
  ) where

import HydraAuctionOnchain.Lib.Serialization (pserializeAddress)
import Plutarch.Api.V2 (PAddress)
import Plutarch.Crypto (pverifyEd25519Signature)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Monadic qualified as P

pmkSigStructure
  :: Term
      s
      ( PBool
          :--> PAddress
          :--> PByteString
          :--> PByteString
          :--> PMaybe PByteString
      )
pmkSigStructure = phoistAcyclic $
  plam $ \isMainnet addr payload payloadLength ->
    pmatch (pserializeAddress # isMainnet # addr) $ \case
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

pverifyCoseSignature
  :: Term
      s
      ( PByteString
          :--> PByteString
          :--> PAddress
          :--> PByteString
          :--> PByteString
          :--> PBool
      )
pverifyCoseSignature = phoistAcyclic $
  plam $ \signature vk addr payload payloadLength -> P.do
    verifyForNetwork <- plet $ plam $ \isMainnet ->
      pmatch (pmkSigStructure # isMainnet # addr # payload # payloadLength) $ \case
        PNothing -> pcon PFalse
        PJust sigStruct ->
          pverifyEd25519Signature # vk # sigStruct # signature
    (verifyForNetwork # pcon PFalse) -- verify for testnet
      #|| (verifyForNetwork # pcon PTrue) -- if verification fails, verify for mainnet
