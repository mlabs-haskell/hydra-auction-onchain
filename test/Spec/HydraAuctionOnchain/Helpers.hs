module Spec.HydraAuctionOnchain.Helpers
  ( hashVerificationKey
  , mkStandingBidTokenValue
  , serialise
  ) where

import Crypto.Hash (Blake2b_224, hash)
import Crypto.PubKey.Ed25519 (PublicKey)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import PlutusLedgerApi.V1 (CurrencySymbol, Value)
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2 (BuiltinByteString, PubKeyHash (PubKeyHash), toBuiltin)
import PlutusTx (ToData, toBuiltinData)
import PlutusTx.Builtins (serialiseData)

hashVerificationKey :: PublicKey -> (BuiltinByteString, PubKeyHash)
hashVerificationKey vkey =
  (vkeyBytes, pubKeyHash)
  where
    vkeyBytes :: BuiltinByteString
    vkeyBytes = toBuiltin @ByteString $ convert vkey

    pubKeyHash :: PubKeyHash
    pubKeyHash =
      PubKeyHash
        . toBuiltin @ByteString
        . convert
        $ hash @PublicKey @Blake2b_224 vkey

mkStandingBidTokenValue :: CurrencySymbol -> Value
mkStandingBidTokenValue cs = Value.singleton cs "STANDING_BID" 1

{-# INLINEABLE serialise #-}
serialise :: ToData a => a -> BuiltinByteString
serialise = serialiseData . toBuiltinData
