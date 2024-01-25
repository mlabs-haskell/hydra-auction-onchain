module HydraAuctionOnchain.Lib.Serialization
  ( pserializeAddress
  ) where

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V2
  ( PAddress
  , PMaybeData (PDJust, PDNothing)
  , PPubKeyHash
  , PScriptHash
  , PStakingCredential (PStakingHash, PStakingPtr)
  )
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Monadic qualified as P

data PAddressConfiguration (s :: S)
  = PAddressConfig'PaymentKeyHash'StakeKeyHash (Term s PPubKeyHash) (Term s PPubKeyHash)
  | PAddressConfig'PaymentKeyHash'ScriptHash (Term s PPubKeyHash) (Term s PScriptHash)
  | PAddressConfig'PaymentKeyHash (Term s PPubKeyHash)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PEq)

instance DerivePlutusType PAddressConfiguration where
  type DPTStrat _ = PlutusTypeScott

data PSerializedAddress (s :: S)
  = PSerializedAddress
      ( Term
          s
          ( PDataRecord
              '[ "addrCbor" ':= PByteString
               , "addrMapEntrySize" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow, PEq)

instance DerivePlutusType PSerializedAddress where
  type DPTStrat _ = PlutusTypeData

pserializeAddress :: Term s (PAddress :--> PMaybe PSerializedAddress)
pserializeAddress = phoistAcyclic $
  plam $ \addr ->
    pmatch (pmkAddrConfig # addr) $ \case
      PNothing -> pnothing
      PJust addrConfig -> P.do
        PPair addrSize addrMapEntrySize <- pmatch $ paddrConfigAddrSize # addrConfig
        addrCbor <-
          plet $
            mconcat
              [ phexByteStr "58" -- byte string (one-byte uint8_t for n, and then n bytes follow)
              , addrSize
              , paddrConfigAddrHeaderForTestnet # addrConfig
              , paddrConfigAddrBody # addrConfig
              ]
        pjust #$ pcon $
          PSerializedAddress $
            pdcons # pdata addrCbor #$ pdcons # pdata addrMapEntrySize # pdnil

pmkAddrConfig :: Term s (PAddress :--> PMaybe PAddressConfiguration)
pmkAddrConfig = phoistAcyclic $
  plam $ \addr -> P.do
    creds <- pletFields @["credential", "stakingCredential"] addr
    pmatch creds.credential $ \case
      PScriptCredential _ -> pnothing
      PPubKeyCredential pkhRec -> P.do
        let pkh = pfield @"_0" # pkhRec
        pmatch creds.stakingCredential $ \case
          PDNothing _ -> pjust # pcon (PAddressConfig'PaymentKeyHash pkh)
          PDJust stakingCredRec ->
            pmatch (pfield @"_0" # stakingCredRec) $ \case
              PStakingHash stakingHashRec ->
                pmatch (pfield @"_0" # stakingHashRec) $ \case
                  PScriptCredential shRec -> P.do
                    let sh = pfield @"_0" # shRec
                    pjust # pcon (PAddressConfig'PaymentKeyHash'ScriptHash pkh sh)
                  PPubKeyCredential skhRec -> P.do
                    let skh = pfield @"_0" # skhRec
                    pjust # pcon (PAddressConfig'PaymentKeyHash'StakeKeyHash pkh skh)
              PStakingPtr _ -> pnothing

-- Get hex-encoded size for given address configuration.
--
-- PaymentKeyHash, StakeKeyHash, and ScriptHash are blake2b-224 hash
-- digests and all have size of 224 bits (28 bytes).
-- https://github.com/cardano-foundation/CIPs/blob/d66f7d0a0bcd06c425a6b7a41c6d18c922deff7e/CIP-0019/README.md?plain=1#L95-L97
paddrConfigAddrSize :: Term s (PAddressConfiguration :--> PPair PByteString PByteString)
paddrConfigAddrSize = phoistAcyclic $
  plam $ \addrConfig ->
    pmatch addrConfig $ \case
      PAddressConfig'PaymentKeyHash'StakeKeyHash _ _ ->
        -- 0x39 = 57 = 1 (header) + 28 (pkh) + 28 (skh)
        pcon $ PPair (phexByteStr "39") (phexByteStr "46")
      PAddressConfig'PaymentKeyHash'ScriptHash _ _ ->
        -- 0x39 = 57 = 1 (header) + 28 (pkh) + 28 (sh)
        pcon $ PPair (phexByteStr "39") (phexByteStr "46")
      PAddressConfig'PaymentKeyHash _ ->
        -- 0x1D = 29 = 1 (header) + 28 (pkh)
        pcon $ PPair (phexByteStr "1D") (phexByteStr "2A")

-- Get hex-encoded network tag + header type for given address
-- configuration.
-- https://github.com/cardano-foundation/CIPs/blob/d66f7d0a0bcd06c425a6b7a41c6d18c922deff7e/CIP-0019/README.md?plain=1#L70-L93
paddrConfigAddrHeaderForTestnet :: Term s (PAddressConfiguration :--> PByteString)
paddrConfigAddrHeaderForTestnet = phoistAcyclic $
  plam $ \addrConfig ->
    pmatch addrConfig $ \case
      PAddressConfig'PaymentKeyHash'StakeKeyHash _ _ ->
        -- 0x00 = 0b0000_0000
        phexByteStr "00"
      PAddressConfig'PaymentKeyHash'ScriptHash _ _ ->
        -- 0x20 = 0b0010_0000
        phexByteStr "20"
      PAddressConfig'PaymentKeyHash _ ->
        -- 0x60 = 0b0110_0000
        phexByteStr "60"

paddrConfigAddrBody :: Term s (PAddressConfiguration :--> PByteString)
paddrConfigAddrBody = phoistAcyclic $
  plam $ \addrConfig ->
    pmatch addrConfig $ \case
      PAddressConfig'PaymentKeyHash'StakeKeyHash pkh skh ->
        pto pkh <> pto skh
      PAddressConfig'PaymentKeyHash'ScriptHash pkh sh ->
        pto pkh <> pto sh
      PAddressConfig'PaymentKeyHash pkh ->
        pto pkh
