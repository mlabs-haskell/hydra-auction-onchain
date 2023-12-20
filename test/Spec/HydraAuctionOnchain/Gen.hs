module Spec.HydraAuctionOnchain.Gen
  ( genTxInfoTemplate
  ) where

import Plutarch.Test.QuickCheck.Instances ()
import PlutusLedgerApi.V1.Interval qualified as Interval (always)
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2 (TxId, TxInfo (..), adaSymbol, adaToken)
import PlutusTx.AssocMap qualified as AMap (empty)
import Test.QuickCheck (Gen, arbitrary)

genTxInfoTemplate :: Gen TxInfo
genTxInfoTemplate = do
  txInfoFeeAda <- arbitrary @Integer
  txInfoId <- arbitrary @TxId
  pure $
    TxInfo
      { txInfoInputs = mempty
      , txInfoReferenceInputs = mempty
      , txInfoOutputs = mempty
      , txInfoFee = Value.singleton adaSymbol adaToken txInfoFeeAda
      , txInfoMint = mempty
      , txInfoDCert = mempty
      , txInfoWdrl = AMap.empty
      , txInfoValidRange = Interval.always
      , txInfoSignatories = mempty
      , txInfoRedeemers = AMap.empty
      , txInfoData = AMap.empty
      , txInfoId
      }
