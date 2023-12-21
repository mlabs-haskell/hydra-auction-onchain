{-# LANGUAGE RecordWildCards #-}

module Spec.HydraAuctionOnchain.Validators.StandingBid (spec) where

import Data.List (singleton)
import HydraAuctionOnchain.Scripts (compileScript)
import HydraAuctionOnchain.Validators.StandingBid (standingBidValidator)
import Plutarch (Script)
import Plutarch.Test.QuickCheck.Instances ()
import Plutarch.Test.QuickCheck.Modifiers
  ( AdaSymbolPresence (WithoutAdaSymbol)
  , GenCurrencySymbol (GenCurrencySymbol)
  )
import PlutusLedgerApi.V2
  ( Address (Address)
  , Credential (ScriptCredential)
  , CurrencySymbol
  , Datum (Datum)
  , OutputDatum (OutputDatum)
  , Redeemer (Redeemer)
  , ScriptContext (..)
  , ScriptHash
  , ScriptPurpose (Spending)
  , TxInInfo (TxInInfo)
  , TxInfo (..)
  , TxOut (..)
  , TxOutRef
  , Value
  , dataToBuiltinData
  , toData
  )
import PlutusTx.AssocMap qualified as AMap (singleton)
import Spec.HydraAuctionOnchain.Expectations (shouldSucceed)
import Spec.HydraAuctionOnchain.Helpers (mkStandingBidTokenValue)
import Spec.HydraAuctionOnchain.QuickCheck.Gen
  ( genKeyPair
  , genTxInfoTemplate
  , genValidAuctionTerms
  , genValidBidState
  , genValidNewBidState
  , vkey
  )
import Spec.HydraAuctionOnchain.Types.AuctionTerms (AuctionTerms)
import Spec.HydraAuctionOnchain.Types.Redeemers (StandingBidRedeemer (NewBidRedeemer))
import Spec.HydraAuctionOnchain.Types.StandingBidState (StandingBidState)
import Test.QuickCheck (Arbitrary (arbitrary), Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

spec :: TestTree
spec =
  testGroup
    "StandingBid"
    [ testGroup
        "NewBidRedeemer"
        [ testProperty "Succeeds" prop_newBid_validInput_succeeds
        ]
    ]

prop_newBid_validInput_succeeds :: TestContext -> Property
prop_newBid_validInput_succeeds testContext = shouldSucceed $ testNewBid testContext

data TestContext = TestContext
  { auctionCs :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , oldBidState :: StandingBidState
  , newBidState :: StandingBidState
  , txInfoTemplate :: TxInfo
  , standingBidInputOref :: TxOutRef
  , scriptAddress :: Address
  }
  deriving stock (Show, Eq)

instance Arbitrary TestContext where
  arbitrary = do
    GenCurrencySymbol auctionCs <- arbitrary @(GenCurrencySymbol 'WithoutAdaSymbol)
    (sellerKeys, bidderKeys) <- (,) <$> genKeyPair <*> genKeyPair
    auctionTerms <- genValidAuctionTerms $ vkey sellerKeys
    oldBidState <- genValidBidState auctionCs auctionTerms sellerKeys bidderKeys
    newBidState <- genValidNewBidState oldBidState auctionCs auctionTerms sellerKeys bidderKeys
    txInfoTemplate <- genTxInfoTemplate
    standingBidInputOref <- arbitrary @TxOutRef
    scriptAddress <- flip Address Nothing . ScriptCredential <$> arbitrary @ScriptHash
    pure TestContext {..}

testNewBid :: TestContext -> Script
testNewBid TestContext {..} =
  let
    standingBidTokenValue :: Value
    standingBidTokenValue = mkStandingBidTokenValue auctionCs

    standingBidInput :: TxInInfo
    standingBidInput =
      TxInInfo standingBidInputOref $
        TxOut
          { txOutAddress = scriptAddress
          , txOutValue = standingBidTokenValue
          , txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ toData oldBidState
          , txOutReferenceScript = Nothing
          }

    standingBidOutput :: TxOut
    standingBidOutput =
      TxOut
        { txOutAddress = scriptAddress
        , txOutValue = standingBidTokenValue
        , txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ toData newBidState
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
              , txInfoOutputs = singleton standingBidOutput
              , txInfoRedeemers = AMap.singleton scriptPurpose redeemer
              }
        , scriptContextPurpose = scriptPurpose
        }
  in
    compile auctionCs auctionTerms oldBidState newBidRedeemer ctx

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
