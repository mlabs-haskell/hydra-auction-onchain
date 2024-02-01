{-# LANGUAGE RecordWildCards #-}

module Spec.HydraAuctionOnchain.QuickCheck.Gen
  ( KeyPair (..)
  , genKeyPair
  , genScriptAddress
  , genTxInfoTemplate
  , genValidAuctionTerms
  , genValidBidState
  , genValidNewBidState
  ) where

import Control.Monad (liftM3)
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, generateSecretKey, sign, toPublic)
import Crypto.Random (drgNewTest, withDRG)
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString (pack)
import Data.Functor ((<&>))
import HydraAuctionOnchain.Types.BidTerms (bidderSigMessageLength)
import Plutarch.Test.QuickCheck.Instances ()
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval qualified as Interval (always)
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2
  ( Address (Address)
  , BuiltinByteString
  , Credential (ScriptCredential)
  , CurrencySymbol
  , POSIXTime (POSIXTime)
  , PubKeyHash
  , ScriptHash
  , TxId
  , TxInfo (..)
  , adaSymbol
  , adaToken
  , toBuiltin
  )
import PlutusTx.AssocMap qualified as AMap (empty)
import PlutusTx.Builtins (lengthOfByteString)
import Spec.HydraAuctionOnchain.Helpers (hashVerificationKey, serialise)
import Spec.HydraAuctionOnchain.QuickCheck.Modifiers (GenNonAdaValue (GenNonAdaValue))
import Spec.HydraAuctionOnchain.Types.AuctionTerms (AuctionTerms (..))
import Spec.HydraAuctionOnchain.Types.BidTerms (BidTerms (..))
import Spec.HydraAuctionOnchain.Types.BidderInfo (BidderInfo (..))
import Spec.HydraAuctionOnchain.Types.StandingBidState (StandingBidState (StandingBidState))
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , NonNegative (NonNegative)
  , Positive (Positive)
  , arbitraryBoundedRandom
  , chooseInt
  , chooseInteger
  , liftArbitrary
  , vector
  )

data KeyPair = KeyPair {skey :: SecretKey, vkey :: PublicKey}
  deriving stock (Show, Eq)

signUsingKeyPair :: ByteArrayAccess ba => KeyPair -> ba -> BuiltinByteString
signUsingKeyPair KeyPair {skey, vkey} message =
  toBuiltin @ByteString $ convert $ sign skey vkey message

genKeyPair :: Gen KeyPair
genKeyPair =
  arbitraryBoundedRandom <&> \seed ->
    let (skey, _) = withDRG (drgNewTest seed) generateSecretKey
    in KeyPair skey $ toPublic skey

genIntegerGreaterThan :: Integer -> Gen Integer
genIntegerGreaterThan a = arbitrary @(Positive Integer) <&> \(Positive b) -> a + b

genScriptAddress :: Gen Address
genScriptAddress = flip Address Nothing . ScriptCredential <$> arbitrary @ScriptHash

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

genValidBidTerms
  :: CurrencySymbol
  -> AuctionTerms
  -> KeyPair
  -> KeyPair
  -> Maybe Integer
  -> Gen BidTerms
genValidBidTerms auctionCs auctionTerms sellerKeys bidderKeys mOldBidPrice = do
  let (bi'BidderVk, bidderPkh) = hashVerificationKey $ vkey bidderKeys
  let bi'BidderAddress = pubKeyHashAddress bidderPkh
  let bt'Bidder = BidderInfo {..}
  bt'BidPrice <- genBidPrice
  let bt'SellerSignature = sellerSignature bi'BidderVk
  let bt'BidderSignature = bidderSignature bt'BidPrice bidderPkh
  pure BidTerms {..}
  where
    genBidPrice :: Gen Integer
    genBidPrice =
      case mOldBidPrice of
        Just oldBidPrice ->
          genIntegerGreaterThan $ oldBidPrice + at'MinBidIncrement auctionTerms
        Nothing ->
          genIntegerGreaterThan $ at'StartingBid auctionTerms

    sellerSignature :: BuiltinByteString -> BuiltinByteString
    sellerSignature bidderVkey =
      signUsingKeyPair sellerKeys $
        (serialise auctionCs <> serialise bidderVkey)

    bidderSignature :: Integer -> PubKeyHash -> BuiltinByteString
    bidderSignature bidPrice bidderPkh =
      signUsingKeyPair bidderKeys $
        padMessage
          bidderSigMessageLength
          (serialise auctionCs <> serialise bidderPkh <> serialise bidPrice)

padMessage :: Integer -> BuiltinByteString -> BuiltinByteString
padMessage targetSize message
  | padSize <= 0 = message
  | otherwise =
      toBuiltin (ByteString.pack $ replicate padSize 0) <> message
  where
    padSize :: Int
    padSize =
      fromInteger $ targetSize - lengthOfByteString message

genValidBidState
  :: CurrencySymbol
  -> AuctionTerms
  -> KeyPair
  -> KeyPair
  -> Gen StandingBidState
genValidBidState auctionCs auctionTerms sellerKeys bidderKeys =
  StandingBidState
    <$> liftArbitrary
      (genValidBidTerms auctionCs auctionTerms sellerKeys bidderKeys Nothing)

genValidNewBidState
  :: StandingBidState
  -> CurrencySymbol
  -> AuctionTerms
  -> KeyPair
  -> KeyPair
  -> Gen StandingBidState
genValidNewBidState oldBidState auctionCs auctionTerms sellerKeys bidderKeys =
  StandingBidState . Just
    <$> genValidBidTerms auctionCs auctionTerms sellerKeys bidderKeys oldBidPrice
  where
    oldBidPrice :: Maybe Integer
    oldBidPrice =
      case oldBidState of
        StandingBidState Nothing -> Nothing
        StandingBidState (Just oldBidTerms) -> Just $ bt'BidPrice oldBidTerms

genValidAuctionTerms :: PublicKey -> Gen AuctionTerms
genValidAuctionTerms vkey = do
  GenNonAdaValue @Positive at'AuctionLot <- arbitrary
  let (at'SellerVk, sellerPkh) = hashVerificationKey vkey
  let at'SellerAddress = pubKeyHashAddress sellerPkh
  at'Delegates <- vector @PubKeyHash =<< chooseInt (1, 10)

  let chooseInterval = POSIXTime <$> chooseInteger (1, 604_800_000) -- up to 1 week in msec
  (biddingPeriod, purchasePeriod, penaltyPeriod) <-
    liftM3 (,,) chooseInterval chooseInterval chooseInterval
  at'BiddingStart <- arbitrary @POSIXTime
  let at'BiddingEnd = at'BiddingStart + biddingPeriod
  let at'PurchaseDeadline = at'BiddingEnd + purchasePeriod
  let at'Cleanup = at'PurchaseDeadline + penaltyPeriod

  let minAuctionFeePerDelegate = 2_000_000
  at'AuctionFeePerDelegate <- genIntegerGreaterThan minAuctionFeePerDelegate

  let minStartingBid = at'AuctionFeePerDelegate * fromIntegral (length at'Delegates)
  at'StartingBid <- genIntegerGreaterThan minStartingBid

  Positive at'MinBidIncrement <- arbitrary @(Positive Integer)
  NonNegative at'MinDepositAmount <- arbitrary @(NonNegative Integer)
  pure AuctionTerms {..}
