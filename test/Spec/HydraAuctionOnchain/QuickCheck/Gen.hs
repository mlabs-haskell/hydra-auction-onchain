{-# LANGUAGE RecordWildCards #-}

module Spec.HydraAuctionOnchain.QuickCheck.Gen
  ( KeyPair (..)
  , genKeyPair
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
import Data.Functor ((<&>))
import Plutarch.Test.QuickCheck.Instances ()
import PlutusLedgerApi.V1.Interval qualified as Interval (always)
import PlutusLedgerApi.V1.Value qualified as Value (singleton)
import PlutusLedgerApi.V2
  ( BuiltinByteString
  , CurrencySymbol
  , POSIXTime (POSIXTime)
  , PubKeyHash
  , TxId
  , TxInfo (..)
  , adaSymbol
  , adaToken
  , toBuiltin
  )
import PlutusTx.AssocMap qualified as AMap (empty)
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
  , suchThat
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

genValidBidTerms :: CurrencySymbol -> AuctionTerms -> KeyPair -> KeyPair -> Gen BidTerms
genValidBidTerms auctionCs auctionTerms sellerKeys bidderKeys = do
  let (bi'BidderVk, bi'BidderPkh) = hashVerificationKey $ vkey bidderKeys
  let bt'Bidder = BidderInfo {..}
  bt'BidPrice <- arbitrary @Integer `suchThat` (\x -> x > at'StartingBid auctionTerms)
  let bt'SellerSignature = sellerSignature bi'BidderVk
  let bt'BidderSignature = bidderSignature bt'BidPrice bi'BidderPkh
  pure BidTerms {..}
  where
    sellerSignature :: BuiltinByteString -> BuiltinByteString
    sellerSignature bidderVkey =
      signUsingKeyPair sellerKeys $
        (serialise auctionCs <> serialise bidderVkey)

    bidderSignature :: Integer -> PubKeyHash -> BuiltinByteString
    bidderSignature bidPrice bidderPkh =
      signUsingKeyPair bidderKeys $
        (serialise auctionCs <> serialise bidderPkh <> serialise bidPrice)

genValidBidState
  :: CurrencySymbol
  -> AuctionTerms
  -> KeyPair
  -> KeyPair
  -> Gen StandingBidState
genValidBidState auctionCs auctionTerms sellerKeys bidderKeys =
  StandingBidState
    <$> liftArbitrary
      (genValidBidTerms auctionCs auctionTerms sellerKeys bidderKeys)

genValidNewBidState
  :: StandingBidState
  -> CurrencySymbol
  -> AuctionTerms
  -> KeyPair
  -> KeyPair
  -> Gen StandingBidState
genValidNewBidState oldBidState auctionCs auctionTerms sellerKeys bidderKeys =
  case oldBidState of
    StandingBidState Nothing ->
      genValidBidState auctionCs auctionTerms sellerKeys bidderKeys
    StandingBidState (Just oldBidTerms) -> do
      newBidPrice <- arbitrary @Integer `suchThat` (\x -> x >= at'MinBidIncrement auctionTerms)
      pure . StandingBidState . Just $
        oldBidTerms
          { bt'BidPrice = newBidPrice
          }

genValidAuctionTerms :: PublicKey -> Gen AuctionTerms
genValidAuctionTerms vkey = do
  GenNonAdaValue @Positive at'AuctionLot <- arbitrary
  let (at'SellerVk, at'SellerPkh) = hashVerificationKey vkey
  at'Delegates <- vector @PubKeyHash =<< chooseInt (0, 10)

  let chooseInterval = POSIXTime <$> chooseInteger (1, 604_800_000) -- up to 1 week in msec
  (biddingPeriod, purchasePeriod, penaltyPeriod) <-
    liftM3 (,,) chooseInterval chooseInterval chooseInterval
  at'BiddingStart <- arbitrary @POSIXTime
  let at'BiddingEnd = at'BiddingStart + biddingPeriod
  let at'PurchaseDeadline = at'BiddingEnd + purchasePeriod
  let at'Cleanup = at'PurchaseDeadline + penaltyPeriod

  let minAuctionFeePerDelegate = 2_000_000
  at'AuctionFeePerDelegate <- arbitrary @Integer `suchThat` (\x -> x > minAuctionFeePerDelegate)

  let minStartingBid = at'AuctionFeePerDelegate * fromIntegral (length at'Delegates)
  at'StartingBid <- arbitrary @Integer `suchThat` (\x -> x > minStartingBid)

  Positive at'MinBidIncrement <- arbitrary @(Positive Integer)
  NonNegative at'MinDepositAmount <- arbitrary @(NonNegative Integer)
  pure AuctionTerms {..}
