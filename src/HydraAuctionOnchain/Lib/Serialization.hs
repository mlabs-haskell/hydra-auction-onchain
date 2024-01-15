module HydraAuctionOnchain.Lib.Serialization
  ( pserializeAddress
  ) where

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V2
  ( PAddress
  , PMaybeData (PDJust, PDNothing)
  , PPubKeyHash
  , PStakingCredential (PStakingHash, PStakingPtr)
  )
import Plutarch.Extra.Maybe (pjust, pnothing)
import Plutarch.Monadic qualified as P

data PAddressConfiguration (s :: S)
  = PAddressConfig'PaymentKeyHash'StakeKeyHash (Term s PPubKeyHash) (Term s PPubKeyHash)
  | PAddressConfig'PaymentKeyHash (Term s PPubKeyHash)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PShow, PEq)

instance DerivePlutusType PAddressConfiguration where
  type DPTStrat _ = PlutusTypeScott

pserializeAddress :: Term s (PAddress :--> PMaybe PByteString)
pserializeAddress = phoistAcyclic $
  plam $ \addr ->
    pmatch (pmkAddrConfig # addr) $ \case
      PNothing -> pnothing
      PJust addrConfig ->
        pjust
          # mconcat
            [ phexByteStr "58" -- byte string (one-byte uint8_t for n, and then n bytes follow)
            , paddrConfigAddrSize # addrConfig
            , paddrConfigAddrHeaderForTestnet # addrConfig
            , paddrConfigAddrBody # addrConfig
            ]

pmkAddrConfig :: Term s (PAddress :--> PMaybe PAddressConfiguration)
pmkAddrConfig = phoistAcyclic $
  plam $ \addr -> P.do
    creds <- pletFields @["credential", "stakingCredential"] addr
    pmatch creds.credential $ \case
      PScriptCredential _ -> pnothing
      PPubKeyCredential rec -> P.do
        let pkh = pfield @"_0" # rec
        pmatch creds.stakingCredential $ \case
          PDNothing _ -> pjust # pcon (PAddressConfig'PaymentKeyHash pkh)
          PDJust rec ->
            pmatch (pfield @"_0" # rec) $ \case
              PStakingHash rec ->
                pmatch (pfield @"_0" # rec) $ \case
                  PScriptCredential _ -> pnothing
                  PPubKeyCredential rec -> P.do
                    let skh = pfield @"_0" # rec
                    pjust # pcon (PAddressConfig'PaymentKeyHash'StakeKeyHash pkh skh)
              PStakingPtr _ -> pnothing

paddrConfigAddrSize :: Term s (PAddressConfiguration :--> PByteString)
paddrConfigAddrSize = phoistAcyclic $
  plam $ \addrConfig ->
    pmatch addrConfig $ \case
      PAddressConfig'PaymentKeyHash'StakeKeyHash _ _ ->
        phexByteStr "39" -- 57 = 1 (header) + 28 (pkh) + 28 (skh)
      PAddressConfig'PaymentKeyHash _ ->
        phexByteStr "1D" -- 29 = 1 (header) + 28 (pkh)

paddrConfigAddrHeaderForTestnet :: Term s (PAddressConfiguration :--> PByteString)
paddrConfigAddrHeaderForTestnet = phoistAcyclic $
  plam $ \addrConfig ->
    pmatch addrConfig $ \case
      PAddressConfig'PaymentKeyHash'StakeKeyHash _ _ ->
        phexByteStr "00" -- 0b0000_0000
      PAddressConfig'PaymentKeyHash _ ->
        phexByteStr "60" -- 0b0110_0000

paddrConfigAddrBody :: Term s (PAddressConfiguration :--> PByteString)
paddrConfigAddrBody = phoistAcyclic $
  plam $ \addrConfig ->
    pmatch addrConfig $ \case
      PAddressConfig'PaymentKeyHash'StakeKeyHash pkh skh ->
        pto pkh <> pto skh
      PAddressConfig'PaymentKeyHash pkh ->
        pto pkh
