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
import PlutusTx.AssocMap qualified as AMap (fromList, singleton)
import Spec.HydraAuctionOnchain.Expectations (shouldFail, shouldFailWithError, shouldSucceed)
import Spec.HydraAuctionOnchain.Helpers
  ( intervalFiniteClosedOpen
  , mkAuctionEscrowTokenValue
  , mkStandingBidTokenValue
  )
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
  ( AuctionEscrowRedeemer
      ( BidderBuysRedeemer
      , CleanupAuctionRedeemer
      , SellerReclaimsRedeemer
      , StartBiddingRedeemer
      )
  , StandingBidRedeemer (ConcludeAuctionRedeemer, MoveToHydraRedeemer, NewBidRedeemer)
  )
import Spec.HydraAuctionOnchain.Types.StandingBidState (StandingBidState (StandingBidState))
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , NonZero (NonZero)
  , Positive (Positive)
  , Property
  , chooseInt
  , elements
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
        [ testProperty "Fails if standing bid input does not exist" $
            prop_missingStandingBidInput_fails
        , testProperty "Fails if there are multiple standing bid inputs" $
            prop_multipleStandingBidInputs_fails
        , testProperty "Fails if standing bid input does not contain standing bid token" $
            prop_standingBidInputMissingToken_fails
        , testProperty "Fails if transaction mints or burns tokens" $
            prop_mintsBurnsValue_fails
        ]
    , testGroup "NewBidRedeemer" $
        -- TODO: Reimplement this test suite in the offchain repo.
        -- testProperty "Succeeds if transaction is valid" $
        --   prop_newBid_validInput_succeeds
        -- testProperty "Fails if tx validity interval is incorrect" $
        --   prop_newBid_incorrectValidRange_fails
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
        ]
    , testGroup "MoveToHydra" $
        [ testProperty "Succeeds if transaction is valid" $
            prop_moveToHydra_validInput_succeeds
        , testProperty "Fails if not all delegates have signed tx" $
            prop_moveToHydra_missingDelegateSigs_fails
        , testProperty "Fails if tx validity interval is incorrect" $
            prop_moveToHydra_incorrectValidRange_fails
        ]
    , testGroup "ConcludeAuction" $
        [ testProperty "Succeeds if transaction is valid" $
            prop_concludeAuction_validInput_succeeds
        , testProperty "Fails if auction escrow input does not exist" $
            prop_concludeAuction_missingAuctionEscrowInput_fails
        , testProperty "Fails if there are multiple auction escrow inputs" $
            prop_concludeAuction_multipleAuctionEscrowInputs_fails
        , testProperty "Fails if auction escrow input does not contain auction escrow token" $
            prop_concludeAuction_auctionEscrowInputMissingToken_fails
        , testProperty "Fails if auction escrow input is being spent with wrong redeemer" $
            prop_concludeAuction_auctionEscrowWrongRedeemer_fails
        ]
    ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- Common ------------------------------------------------------------

prop_missingStandingBidInput_fails :: NewBidTestContext -> Property
prop_missingStandingBidInput_fails testContext =
  shouldFailWithError StandingBid'Error'MissingStandingBidInput $
    testNewBid testContext $
      def
        { standingBidInputMode = TxOutputMissing
        }

prop_multipleStandingBidInputs_fails :: NewBidTestContext -> Property
prop_multipleStandingBidInputs_fails testContext =
  shouldFailWithError StandingBid'Error'TooManyOwnScriptInputs $
    testNewBid testContext $
      def
        { standingBidInputMode = MultipleTxOutputs
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

_prop_newBid_validInput_succeeds :: NewBidTestContext -> Property
_prop_newBid_validInput_succeeds testContext =
  shouldSucceed $
    testNewBid testContext def

prop_newBid_missingStandingBidOutput_fails :: NewBidTestContext -> Property
prop_newBid_missingStandingBidOutput_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'MissingOwnOutput $
    testNewBid testContext $
      def
        { standingBidOutputMode = TxOutputMissing
        }

prop_multipleStandingBidOutputs_fails :: NewBidTestContext -> Property
prop_multipleStandingBidOutputs_fails testContext =
  shouldFailWithError StandingBid'NewBid'Error'MissingOwnOutput $
    testNewBid testContext $
      def
        { standingBidOutputMode = MultipleTxOutputs
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

_prop_newBid_incorrectValidRange_fails :: NewBidTestContext -> Property
_prop_newBid_incorrectValidRange_fails testContext =
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

-- ConcludeAuction ---------------------------------------------------

prop_concludeAuction_validInput_succeeds :: ConcludeAuctionTestContext -> Property
prop_concludeAuction_validInput_succeeds testContext =
  shouldSucceed $
    testConcludeAuction testContext def

prop_concludeAuction_missingAuctionEscrowInput_fails :: ConcludeAuctionTestContext -> Property
prop_concludeAuction_missingAuctionEscrowInput_fails testContext =
  shouldFailWithError StandingBid'ConcludeAuction'Error'MissingAuctionEscrowInput $
    testConcludeAuction testContext $
      def
        { ca'auctionEscrowInputMode = TxOutputMissing
        }

prop_concludeAuction_multipleAuctionEscrowInputs_fails
  :: ConcludeAuctionTestContext -> Property
prop_concludeAuction_multipleAuctionEscrowInputs_fails testContext =
  shouldFailWithError StandingBid'ConcludeAuction'Error'MissingAuctionEscrowInput $
    testConcludeAuction testContext $
      def
        { ca'auctionEscrowInputMode = MultipleTxOutputs
        }

prop_concludeAuction_auctionEscrowInputMissingToken_fails
  :: ConcludeAuctionTestContext -> Property
prop_concludeAuction_auctionEscrowInputMissingToken_fails testContext =
  shouldFailWithError StandingBid'ConcludeAuction'Error'MissingAuctionEscrowInput $
    testConcludeAuction testContext $
      def
        { ca'auctionEscrowInputContainsToken = False
        }

prop_concludeAuction_auctionEscrowWrongRedeemer_fails :: ConcludeAuctionTestContext -> Property
prop_concludeAuction_auctionEscrowWrongRedeemer_fails testContext =
  shouldFailWithError StandingBid'ConcludeAuction'Error'InvalidAuctionEscrowRedeemer $
    testConcludeAuction testContext $
      def
        { ca'spendsAuctionEscrowInputWithWrongRedeemer = True
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

-- ConcludeAuction ---------------------------------------------------

data ConcludeAuctionTestContext = ConcludeAuctionTestContext
  { auctionCs :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , txInfoTemplate :: TxInfo
  , standingBidInputOref :: TxOutRef
  , auctionEscrowInputOref :: TxOutRef
  , standingBidAddress :: Address
  , auctionEscrowAddress :: Address
  , auctionEscrowRedeemer :: AuctionEscrowRedeemer
  , auctionEscrowWrongRedeemer :: AuctionEscrowRedeemer
  }
  deriving stock (Show, Eq)

instance Arbitrary ConcludeAuctionTestContext where
  arbitrary = do
    GenCurrencySymbol auctionCs <- arbitrary @(GenCurrencySymbol 'WithoutAdaSymbol)
    sellerKeys <- genKeyPair
    auctionTerms <- genValidAuctionTerms $ vkey sellerKeys
    txInfoTemplate <- genTxInfoTemplate
    standingBidInputOref <- arbitrary @TxOutRef
    auctionEscrowInputOref <- arbitrary @TxOutRef
    standingBidAddress <- genScriptAddress
    auctionEscrowAddress <- genScriptAddress
    auctionEscrowRedeemer <- elements [BidderBuysRedeemer, SellerReclaimsRedeemer]
    auctionEscrowWrongRedeemer <- elements [StartBiddingRedeemer, CleanupAuctionRedeemer]
    pure ConcludeAuctionTestContext {..}

--------------------------------------------------------------------------------
-- TestConstraints
--------------------------------------------------------------------------------

data TxOutputMode = TxOutputValid | TxOutputMissing | MultipleTxOutputs
  deriving stock (Show, Eq)

data NewBidStateMode
  = NewBidStateValid
  | NewBidStateEmpty
  | NewBidStateMissingDatum
  | NewBidStateInvalidDatum
  deriving stock (Show, Eq)

-- NewBid ------------------------------------------------------------

data NewBidTestConstraints = NewBidTestConstraints
  { standingBidInputMode :: TxOutputMode
  , standingBidInputContainsToken :: Bool
  , mintsBurnsValue :: Bool
  , standingBidOutputMode :: TxOutputMode
  , standingBidOutputContainsToken :: Bool
  , newBidStateMode :: NewBidStateMode
  , txHasIncorrectValidRange :: Bool
  }
  deriving stock (Show, Eq)

instance Default NewBidTestConstraints where
  def =
    NewBidTestConstraints
      { standingBidInputMode = TxOutputValid
      , standingBidInputContainsToken = True
      , mintsBurnsValue = False
      , standingBidOutputMode = TxOutputValid
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

-- ConcludeAuction ---------------------------------------------------

data ConcludeAuctionTestConstraints = ConcludeAuctionTestConstraints
  { ca'auctionEscrowInputMode :: TxOutputMode
  , ca'auctionEscrowInputContainsToken :: Bool
  , ca'spendsAuctionEscrowInputWithWrongRedeemer :: Bool
  }
  deriving stock (Show, Eq)

instance Default ConcludeAuctionTestConstraints where
  def =
    ConcludeAuctionTestConstraints
      { ca'auctionEscrowInputMode = TxOutputValid
      , ca'auctionEscrowInputContainsToken = True
      , ca'spendsAuctionEscrowInputWithWrongRedeemer = False
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
                    TxOutputValid -> singleton standingBidInput
                    TxOutputMissing -> mempty
                    MultipleTxOutputs -> replicate 2 standingBidInput
              , txInfoOutputs =
                  case standingBidOutputMode of
                    TxOutputValid -> singleton standingBidOutput
                    TxOutputMissing -> mempty
                    MultipleTxOutputs -> replicate 2 standingBidOutput
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
    bidState :: StandingBidState
    bidState = StandingBidState Nothing

    standingBidInput :: TxInInfo
    standingBidInput =
      TxInInfo standingBidInputOref $
        TxOut
          { txOutAddress = scriptAddress
          , txOutValue = mkStandingBidTokenValue auctionCs
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

-- ConcludeAuction ---------------------------------------------------

testConcludeAuction :: ConcludeAuctionTestContext -> ConcludeAuctionTestConstraints -> Script
testConcludeAuction ConcludeAuctionTestContext {..} ConcludeAuctionTestConstraints {..} =
  let
    bidState :: StandingBidState
    bidState = StandingBidState Nothing

    standingBidInput :: TxInInfo
    standingBidInput =
      TxInInfo standingBidInputOref $
        TxOut
          { txOutAddress = standingBidAddress
          , txOutValue = mkStandingBidTokenValue auctionCs
          , txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ toData bidState
          , txOutReferenceScript = Nothing
          }

    auctionEscrowTokenValue :: Value
    auctionEscrowTokenValue = mkAuctionEscrowTokenValue auctionCs

    auctionEscrowInput :: TxInInfo
    auctionEscrowInput =
      TxInInfo auctionEscrowInputOref $
        TxOut
          { txOutAddress = auctionEscrowAddress
          , txOutValue =
              bool mempty auctionEscrowTokenValue ca'auctionEscrowInputContainsToken
          , txOutDatum = NoOutputDatum
          , txOutReferenceScript = Nothing
          }

    concludeAuctionRedeemer :: StandingBidRedeemer
    concludeAuctionRedeemer = ConcludeAuctionRedeemer

    redeemer :: Redeemer
    redeemer = Redeemer $ dataToBuiltinData $ toData concludeAuctionRedeemer

    auctionEscrowRedeemer' :: Redeemer
    auctionEscrowRedeemer' =
      Redeemer . dataToBuiltinData . toData $
        bool
          auctionEscrowRedeemer
          auctionEscrowWrongRedeemer
          ca'spendsAuctionEscrowInputWithWrongRedeemer

    scriptContextPurpose :: ScriptPurpose
    scriptContextPurpose = Spending standingBidInputOref

    ctx :: ScriptContext
    ctx =
      ScriptContext
        { scriptContextTxInfo =
            txInfoTemplate
              { txInfoInputs =
                  standingBidInput
                    : case ca'auctionEscrowInputMode of
                      TxOutputValid -> singleton auctionEscrowInput
                      TxOutputMissing -> mempty
                      MultipleTxOutputs -> replicate 2 auctionEscrowInput
              , txInfoRedeemers =
                  AMap.fromList
                    [ (scriptContextPurpose, redeemer)
                    , (Spending auctionEscrowInputOref, auctionEscrowRedeemer')
                    ]
              }
        , scriptContextPurpose
        }
  in
    compile auctionCs auctionTerms bidState concludeAuctionRedeemer ctx

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
