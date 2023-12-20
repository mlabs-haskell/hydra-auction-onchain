{-# LANGUAGE RecordWildCards #-}

module Spec.HydraAuctionOnchain.Validators.StandingBid (spec) where

import Data.List (singleton)
import HydraAuctionOnchain.Scripts (compileScript)
import HydraAuctionOnchain.Validators.StandingBid (standingBidValidator)
import Plutarch (Script)
import PlutusLedgerApi.V2
  ( Address
  , CurrencySymbol
  , Datum (Datum)
  , OutputDatum (OutputDatum)
  , Redeemer (Redeemer)
  , ScriptContext (..)
  , ScriptPurpose (Spending)
  , TxInInfo (TxInInfo)
  , TxInfo (..)
  , TxOut (..)
  , TxOutRef
  , dataToBuiltinData
  , toData
  )
import PlutusTx.AssocMap qualified as AMap (singleton)
import Spec.HydraAuctionOnchain.Gen (genTxInfoTemplate)
import Spec.HydraAuctionOnchain.Helpers (shouldFail, shouldSucceed)
import Spec.HydraAuctionOnchain.Types.AuctionTerms (AuctionTerms)
import Spec.HydraAuctionOnchain.Types.Redeemers (StandingBidRedeemer (NewBidRedeemer))
import Spec.HydraAuctionOnchain.Types.StandingBidState (StandingBidState)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)

spec :: TestTree
spec =
  testGroup
    "StandingBid"
    [ testProperty "Succeeds" prop_validInput_succeeds
    , testProperty "Fails when own input is missing" prop_ownInputMissing_fails
    ]

prop_validInput_succeeds :: Property
prop_validInput_succeeds = shouldSucceed undefined

prop_ownInputMissing_fails :: Property
prop_ownInputMissing_fails = shouldFail undefined

data TestContext = TestContext
  { auctionCs :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , datum :: StandingBidState
  , txInfoTemplate :: TxInfo
  , standingBidInputOref :: TxOutRef
  , scriptAddress :: Address
  }

testNewBid :: TestContext -> Script
testNewBid TestContext {..} =
  let
    standingBidInput :: TxInInfo
    standingBidInput =
      TxInInfo standingBidInputOref $
        TxOut
          { txOutAddress = scriptAddress
          , txOutValue = undefined
          , txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ toData datum
          , txOutReferenceScript = Nothing
          }

    newBidRedeemer :: StandingBidRedeemer
    newBidRedeemer = NewBidRedeemer

    redeemer :: Redeemer
    redeemer = Redeemer $ dataToBuiltinData $ toData newBidRedeemer

    scriptPurpose :: ScriptPurpose
    scriptPurpose = Spending standingBidInputOref

    ctx :: ScriptContext
    ctx =
      ScriptContext
        { scriptContextTxInfo =
            txInfoTemplate
              { txInfoInputs = singleton standingBidInput
              , txInfoOutputs = outputs
              , txInfoRedeemers = AMap.singleton scriptPurpose redeemer
              }
        , scriptContextPurpose = scriptPurpose
        }
  in
    compile auctionCs auctionTerms datum newBidRedeemer ctx

compile
  :: CurrencySymbol
  -> AuctionTerms
  -> StandingBidState
  -> StandingBidRedeemer
  -> ScriptContext
  -> Script
compile auctionCs auctionTerms datum redeemer ctx =
  compileScript $
    popaque $
      standingBidValidator
        # pconstant auctionCs
        # pconstant auctionTerms
        # pconstant datum
        # pconstant redeemer
        # pconstant ctx
