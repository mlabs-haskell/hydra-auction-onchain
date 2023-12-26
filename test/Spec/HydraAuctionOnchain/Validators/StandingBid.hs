{-# LANGUAGE RecordWildCards #-}

module Spec.HydraAuctionOnchain.Validators.StandingBid (spec) where

import Data.Bool (bool)
import Data.Default (Default (def))
import Data.List (singleton)
import HydraAuctionOnchain.Errors.StandingBid (PStandingBidError (..))
import HydraAuctionOnchain.Scripts (compileScript)
import HydraAuctionOnchain.Validators.StandingBid (standingBidValidator)
import Plutarch (Script)
import Plutarch.Test.QuickCheck.Instances ()
import Plutarch.Test.QuickCheck.Modifiers
  ( AdaSymbolPresence (WithoutAdaSymbol)
  , GenCurrencySymbol (GenCurrencySymbol)
  )
import PlutusLedgerApi.V2
  ( Address
  , CurrencySymbol
  , Datum (Datum)
  , OutputDatum (NoOutputDatum, OutputDatum)
  , POSIXTime
  , POSIXTimeRange
  , PubKeyHash
  , Redeemer (Redeemer)
  , ScriptContext (..)
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
import Spec.HydraAuctionOnchain.Expectations (shouldFail, shouldFailWithError, shouldSucceed)
import Spec.HydraAuctionOnchain.Helpers (intervalFiniteClosedOpen, mkStandingBidTokenValue)
import Spec.HydraAuctionOnchain.QuickCheck.Gen
  ( genKeyPair
  , genScriptAddress
  , genTxInfoTemplate
  , genValidAuctionTerms
  , genValidBidState
  , genValidNewBidState
  , vkey
  )
import Spec.HydraAuctionOnchain.QuickCheck.Modifiers (GenNonAdaValue (GenNonAdaValue))
import Spec.HydraAuctionOnchain.Types.AuctionTerms (AuctionTerms (..), biddingPeriod)
import Spec.HydraAuctionOnchain.Types.Redeemers
  ( StandingBidRedeemer (MoveToHydraRedeemer, NewBidRedeemer)
  )
import Spec.HydraAuctionOnchain.Types.StandingBidState (StandingBidState (StandingBidState))
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , NonZero (NonZero)
  , Positive (Positive)
  , Property
  , chooseInt
  , resize
  , shuffle
  , suchThat
  )
import Test.Tasty (TestTree, testGroup)

-- import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.QuickCheck (testProperty)

spec :: TestTree
spec =
  testGroup "StandingBid" $
    [ testGroup "Common" $
        [ testProperty "Succeeds if transaction is valid" $
            prop_validInput_succeeds
        , testProperty "Fails if standing bid input does not exist" $
            prop_missingStandingBidInput_fails
        , testProperty "Fails if there are multiple standing bid inputs" $
            prop_multipleStandingBidInputs_fails
        , testProperty "Fails if standing bid input does not contain standing bid token" $
            prop_standingBidInputMissingToken_fails
        , testProperty "Fails if transaction mints or burns tokens" $
            prop_mintsBurnsValue_fails
        ]
    , testGroup "NewBidRedeemer" $
        [ testProperty "Fails if standing bid output does not exist" $
            prop_newBid_missingStandingBidOutput_fails
        , testProperty "Fails if there are multiple standing bid outputs" $
            prop_multipleStandingBidOutputs_fails
        , testProperty "Fails if standing bid output does not contain standing bid token" $
            prop_newBid_standingBidOutputMissingToken_fails
        , testProperty "Fails if new bid state datum is missing" $
            prop_newBid_missingNewBidStateDatum_fails
        , testProperty "Fails if new bid state datum is invalid" $
            prop_newBid_invalidNewBidStateDatum_fails
        , testProperty "Fails if new bid state is empty" $
            prop_newBid_emptyNewBidState_fails
        , testProperty "Fails if tx validity interval is incorrect" $
            prop_newBid_incorrectValidRange_fails
        ]
    , testGroup "MoveToHydra" $
        [ testProperty "Succeeds if transaction is valid" $
            prop_moveToHydra_validInput_succeeds
        , testProperty "Fails if not all delegates have signed tx" $
            prop_moveToHydra_missingDelegateSigs_fails
        , testProperty "Fails if tx validity interval is incorrect" $
            prop_moveToHydra_incorrectValidRange_fails
        ]
    ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- Common ------------------------------------------------------------

prop_validInput_succeeds :: NewBidTestContext -> Property
prop_validInput_succeeds testContext =
  shouldSucceed $
    testNewBid testContext def

prop_missingStandingBidInput_fails :: NewBidTestContext -> Property
prop_missingStandingBidInput_fails testContext =
  shouldFailWithError StandingBid'Error'MissingStandingBidInput $
    testNewBid testContext $
      def
        { standingBidInputMode = StandingBidInputMissing
        }

prop_multipleStandingBidInputs_fails :: NewBidTestContext -> Property
prop_multipleStandingBidInputs_fails testContext =
  shouldFailWithError StandingBid'Error'TooManyOwnScriptInputs $
    testNewBid testContext $
      def
        { standingBidInputMode = MultipleStandingBidInputs
        }

prop_standingBidInputMissingToken_fails :: NewBidTestContext -> Property
prop_standingBidInputMissingToken_fails testContext =
  shouldFailWithError StandingBid'Error'OwnInputMissingToken $
    testNewBid testContext $
      def
        { standingBidInputContainsToken = False
        }

prop_mintsBurnsValue_fails :: NewBidTestContext -> Property
prop_mintsBurnsValue_fails testContext =
  shouldFailWithError StandingBid'Error'UnexpectedTokensMintedBurned $
    testNewBid testContext $
      def
        { mintsBurnsValue = True
        }

-- NewBid ------------------------------------------------------------

prop_newBid_missingStandingBidOutput_fails :: NewBidTestContext -> Property
prop_newBid_missingStandingBidOutput_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'MissingOwnOutput $
    testNewBid testContext $
      def
        { standingBidOutputMode = StandingBidOutputMissing
        }

prop_multipleStandingBidOutputs_fails :: NewBidTestContext -> Property
prop_multipleStandingBidOutputs_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'MissingOwnOutput $
    testNewBid testContext $
      def
        { standingBidOutputMode = MultipleStandingBidOutputs
        }

prop_newBid_standingBidOutputMissingToken_fails :: NewBidTestContext -> Property
prop_newBid_standingBidOutputMissingToken_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'OwnOutputMissingToken $
    testNewBid testContext $
      def
        { standingBidOutputContainsToken = False
        }

prop_newBid_missingNewBidStateDatum_fails :: NewBidTestContext -> Property
prop_newBid_missingNewBidStateDatum_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'FailedToDecodeNewBid $
    testNewBid testContext $
      def
        { newBidStateMode = NewBidStateMissingDatum
        }

prop_newBid_invalidNewBidStateDatum_fails :: NewBidTestContext -> Property
prop_newBid_invalidNewBidStateDatum_fails testContext =
  shouldFail $
    testNewBid testContext $
      def
        { newBidStateMode = NewBidStateInvalidDatum
        }

prop_newBid_emptyNewBidState_fails :: NewBidTestContext -> Property
prop_newBid_emptyNewBidState_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'InvalidNewBidState $
    testNewBid testContext $
      def
        { newBidStateMode = NewBidStateEmpty
        }

prop_newBid_incorrectValidRange_fails :: NewBidTestContext -> Property
prop_newBid_incorrectValidRange_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'IncorrectValidityInterval $
    testNewBid testContext $
      def
        { txHasIncorrectValidRange = True
        }

-- MoveToHydra -------------------------------------------------------

prop_moveToHydra_validInput_succeeds :: MoveToHydraTestContext -> Property
prop_moveToHydra_validInput_succeeds testContext =
  shouldSucceed $
    testMoveToHydra testContext def

prop_moveToHydra_missingDelegateSigs_fails :: MoveToHydraTestContext -> Property
prop_moveToHydra_missingDelegateSigs_fails testContext =
  shouldFailWithError StandingBid'MoveToHydra'Error'MissingDelegateSignatures $
    testMoveToHydra testContext $
      def
        { mh'txSignedByAllDelegates = False
        }

prop_moveToHydra_incorrectValidRange_fails :: MoveToHydraTestContext -> Property
prop_moveToHydra_incorrectValidRange_fails testContext =
  shouldFailWithError StandingBid'MoveToHydra'Error'IncorrectValidityInterval $
    testMoveToHydra testContext $
      def
        { mh'txHasIncorrectValidRange = True
        }

--------------------------------------------------------------------------------
-- TestContext
--------------------------------------------------------------------------------

-- NewBid ------------------------------------------------------------

data NewBidTestContext = NewBidTestContext
  { auctionCs :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , oldBidState :: StandingBidState
  , newBidState :: StandingBidState
  , txInfoTemplate :: TxInfo
  , standingBidInputOref :: TxOutRef
  , scriptAddress :: Address
  , invalidNewBidStateDatum :: Datum
  , invalidMintValue :: Value
  , incorrectTxValidRange :: POSIXTimeRange
  }
  deriving stock (Show, Eq)

instance Arbitrary NewBidTestContext where
  arbitrary = do
    GenCurrencySymbol auctionCs <- arbitrary @(GenCurrencySymbol 'WithoutAdaSymbol)
    (sellerKeys, bidderKeys) <- (,) <$> genKeyPair <*> genKeyPair
    auctionTerms <- genValidAuctionTerms $ vkey sellerKeys
    oldBidState <- genValidBidState auctionCs auctionTerms sellerKeys bidderKeys
    newBidState <- genValidNewBidState oldBidState auctionCs auctionTerms sellerKeys bidderKeys
    txInfoTemplate <- genTxInfoTemplate
    standingBidInputOref <- arbitrary @TxOutRef
    scriptAddress <- genScriptAddress
    invalidNewBidStateDatum <- resize 10 $ arbitrary @Datum
    GenNonAdaValue @NonZero invalidMintValue <- arbitrary `suchThat` ((/=) mempty)
    Positive @POSIXTime validRangeDelta <- arbitrary
    let incorrectTxValidRange =
          intervalFiniteClosedOpen
            (at'BiddingStart auctionTerms + validRangeDelta)
            (at'BiddingEnd auctionTerms + validRangeDelta)
    pure NewBidTestContext {..}

-- MoveToHydra -------------------------------------------------------

data MoveToHydraTestContext = MoveToHydraTestContext
  { auctionCs :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , txInfoTemplate :: TxInfo
  , standingBidInputOref :: TxOutRef
  , scriptAddress :: Address
  , incorrectTxValidRange :: POSIXTimeRange
  , delegatesSublist :: [PubKeyHash]
  }
  deriving stock (Show, Eq)

instance Arbitrary MoveToHydraTestContext where
  arbitrary = do
    GenCurrencySymbol auctionCs <- arbitrary @(GenCurrencySymbol 'WithoutAdaSymbol)
    sellerKeys <- genKeyPair
    auctionTerms <- genValidAuctionTerms $ vkey sellerKeys
    txInfoTemplate <- genTxInfoTemplate
    standingBidInputOref <- arbitrary @TxOutRef
    scriptAddress <- genScriptAddress
    Positive @POSIXTime validRangeDelta <- arbitrary
    let incorrectTxValidRange =
          intervalFiniteClosedOpen
            (at'BiddingStart auctionTerms + validRangeDelta)
            (at'BiddingEnd auctionTerms + validRangeDelta)
    delegates' <- shuffle $ at'Delegates auctionTerms
    delegatesSublist <- flip take delegates' <$> chooseInt (0, length delegates' - 1)
    pure MoveToHydraTestContext {..}

--------------------------------------------------------------------------------
-- TestConstraints
--------------------------------------------------------------------------------

data StandingBidInputMode
  = StandingBidInputValid
  | StandingBidInputMissing
  | MultipleStandingBidInputs
  deriving stock (Show, Eq)

data StandingBidOutputMode
  = StandingBidOutputValid
  | StandingBidOutputMissing
  | MultipleStandingBidOutputs
  deriving stock (Show, Eq)

data NewBidStateMode
  = NewBidStateValid
  | NewBidStateEmpty
  | NewBidStateMissingDatum
  | NewBidStateInvalidDatum
  deriving stock (Show, Eq)

-- NewBid ------------------------------------------------------------

data NewBidTestConstraints = NewBidTestConstraints
  { standingBidInputMode :: StandingBidInputMode
  , standingBidInputContainsToken :: Bool
  , mintsBurnsValue :: Bool
  , standingBidOutputMode :: StandingBidOutputMode
  , standingBidOutputContainsToken :: Bool
  , newBidStateMode :: NewBidStateMode
  , txHasIncorrectValidRange :: Bool
  }
  deriving stock (Show, Eq)

instance Default NewBidTestConstraints where
  def =
    NewBidTestConstraints
      { standingBidInputMode = StandingBidInputValid
      , standingBidInputContainsToken = True
      , mintsBurnsValue = False
      , standingBidOutputMode = StandingBidOutputValid
      , standingBidOutputContainsToken = True
      , newBidStateMode = NewBidStateValid
      , txHasIncorrectValidRange = False
      }

-- MoveToHydra -------------------------------------------------------

data MoveToHydraTestConstraints = MoveToHydraTestConstraints
  { mh'txSignedByAllDelegates :: Bool
  , mh'txHasIncorrectValidRange :: Bool
  }
  deriving stock (Show, Eq)

instance Default MoveToHydraTestConstraints where
  def =
    MoveToHydraTestConstraints
      { mh'txSignedByAllDelegates = True
      , mh'txHasIncorrectValidRange = False
      }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- NewBid ------------------------------------------------------------

testNewBid :: NewBidTestContext -> NewBidTestConstraints -> Script
testNewBid NewBidTestContext {..} NewBidTestConstraints {..} =
  let
    standingBidTokenValue :: Value
    standingBidTokenValue = mkStandingBidTokenValue auctionCs

    standingBidInput :: TxInInfo
    standingBidInput =
      TxInInfo standingBidInputOref $
        TxOut
          { txOutAddress = scriptAddress
          , txOutValue = bool mempty standingBidTokenValue standingBidInputContainsToken
          , txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ toData oldBidState
          , txOutReferenceScript = Nothing
          }

    standingBidOutput :: TxOut
    standingBidOutput =
      TxOut
        { txOutAddress = scriptAddress
        , txOutValue = bool mempty standingBidTokenValue standingBidOutputContainsToken
        , txOutDatum =
            case newBidStateMode of
              NewBidStateValid ->
                OutputDatum $ Datum $ dataToBuiltinData $ toData newBidState
              NewBidStateEmpty ->
                OutputDatum $ Datum $ dataToBuiltinData $ toData $ StandingBidState Nothing
              NewBidStateMissingDatum ->
                NoOutputDatum
              NewBidStateInvalidDatum ->
                OutputDatum invalidNewBidStateDatum
        , txOutReferenceScript = Nothing
        }

    newBidRedeemer :: StandingBidRedeemer
    newBidRedeemer = NewBidRedeemer

    redeemer :: Redeemer
    redeemer = Redeemer $ dataToBuiltinData $ toData newBidRedeemer

    scriptContextPurpose :: ScriptPurpose
    scriptContextPurpose = Spending standingBidInputOref

    txInfoValidRange :: POSIXTimeRange
    txInfoValidRange =
      bool (biddingPeriod auctionTerms) incorrectTxValidRange txHasIncorrectValidRange

    ctx :: ScriptContext
    ctx =
      ScriptContext
        { scriptContextTxInfo =
            txInfoTemplate
              { txInfoInputs =
                  case standingBidInputMode of
                    StandingBidInputValid -> singleton standingBidInput
                    StandingBidInputMissing -> mempty
                    MultipleStandingBidInputs -> replicate 2 standingBidInput
              , txInfoOutputs =
                  case standingBidOutputMode of
                    StandingBidOutputValid -> singleton standingBidOutput
                    StandingBidOutputMissing -> mempty
                    MultipleStandingBidOutputs -> replicate 2 standingBidOutput
              , txInfoMint = bool mempty invalidMintValue mintsBurnsValue
              , txInfoValidRange
              , txInfoRedeemers = AMap.singleton scriptContextPurpose redeemer
              }
        , scriptContextPurpose
        }
  in
    compile auctionCs auctionTerms oldBidState newBidRedeemer ctx

-- MoveToHydra -------------------------------------------------------

testMoveToHydra :: MoveToHydraTestContext -> MoveToHydraTestConstraints -> Script
testMoveToHydra MoveToHydraTestContext {..} MoveToHydraTestConstraints {..} =
  let
    standingBidTokenValue :: Value
    standingBidTokenValue = mkStandingBidTokenValue auctionCs

    bidState :: StandingBidState
    bidState = StandingBidState Nothing

    standingBidInput :: TxInInfo
    standingBidInput =
      TxInInfo standingBidInputOref $
        TxOut
          { txOutAddress = scriptAddress
          , txOutValue = standingBidTokenValue
          , txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ toData bidState
          , txOutReferenceScript = Nothing
          }

    moveToHydraRedeemer :: StandingBidRedeemer
    moveToHydraRedeemer = MoveToHydraRedeemer

    redeemer :: Redeemer
    redeemer = Redeemer $ dataToBuiltinData $ toData moveToHydraRedeemer

    scriptContextPurpose :: ScriptPurpose
    scriptContextPurpose = Spending standingBidInputOref

    txInfoValidRange :: POSIXTimeRange
    txInfoValidRange =
      bool (biddingPeriod auctionTerms) incorrectTxValidRange mh'txHasIncorrectValidRange

    txInfoSignatories :: [PubKeyHash]
    txInfoSignatories =
      bool delegatesSublist (at'Delegates auctionTerms) mh'txSignedByAllDelegates

    ctx :: ScriptContext
    ctx =
      ScriptContext
        { scriptContextTxInfo =
            txInfoTemplate
              { txInfoInputs = singleton standingBidInput
              , txInfoValidRange
              , txInfoSignatories
              , txInfoRedeemers = AMap.singleton scriptContextPurpose redeemer
              }
        , scriptContextPurpose
        }
  in
    compile auctionCs auctionTerms bidState moveToHydraRedeemer ctx

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

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
