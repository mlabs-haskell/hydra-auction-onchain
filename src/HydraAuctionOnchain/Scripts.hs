module HydraAuctionOnchain.Scripts
  ( auctionEscrowValidatorScript
  , auctionEscrowValidatorUntyped
  , auctionMetadataValidatorScript
  , auctionMetadataValidatorUntyped
  , auctionMintingPolicyScript
  , auctionMintingPolicyUntyped
  , bidderDepositValidatorScript
  , bidderDepositValidatorUntyped
  , compileScript
  , standingBidValidatorScript
  , standingBidValidatorUntyped
  , writeScript
  ) where

import Data.Text (Text)
import Data.Text qualified as T (unpack)
import HydraAuctionOnchain.MintingPolicies.Auction (auctionMintingPolicy)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import HydraAuctionOnchain.Types.Scripts
  ( PAuctionEscrowScriptHash
  , PFeeEscrowScriptHash
  , PStandingBidScriptHash
  )
import HydraAuctionOnchain.Validators.AuctionEscrow (auctionEscrowValidator)
import HydraAuctionOnchain.Validators.AuctionMetadata (auctionMetadataValidator)
import HydraAuctionOnchain.Validators.BidderDeposit (bidderDepositValidator)
import HydraAuctionOnchain.Validators.StandingBid (standingBidValidator)
import Plutarch (Config (Config), Script, TracingMode (DoTracing), compile)
import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PScriptHash, PTxOutRef, PValidator)
import Plutarch.Unsafe (punsafeCoerce)
import Ply.Plutarch.TypedWriter (TypedWriter, writeTypedScript)

----------------------------------------------------------------------
-- Auction MP

auctionMintingPolicyUntyped
  :: ClosedTerm
      ( PAsData PScriptHash
          :--> PAsData PTxOutRef
          :--> PMintingPolicy
      )
auctionMintingPolicyUntyped = phoistAcyclic $
  plam $ \auctionMetadataSh nonceOref redeemer ctx ->
    popaque $
      auctionMintingPolicy
        # pfromData auctionMetadataSh
        # pfromData nonceOref
        # punsafeCoerce redeemer
        # ctx

auctionMintingPolicyScript :: Script
auctionMintingPolicyScript = compileScript auctionMintingPolicyUntyped

----------------------------------------------------------------------
-- AuctionEscrow

auctionEscrowValidatorUntyped
  :: ClosedTerm
      ( PAsData PStandingBidScriptHash
          :--> PAsData PFeeEscrowScriptHash
          :--> PAsData PCurrencySymbol
          :--> PAsData PAuctionTerms
          :--> PValidator
      )
auctionEscrowValidatorUntyped = phoistAcyclic $
  plam $ \standingBidSh feeEscrowSh auctionCs auctionTerms datum redeemer ctx ->
    popaque $
      auctionEscrowValidator
        # pfromData standingBidSh
        # pfromData feeEscrowSh
        # pfromData auctionCs
        # pfromData auctionTerms
        # punsafeCoerce datum
        # punsafeCoerce redeemer
        # ctx

auctionEscrowValidatorScript :: Script
auctionEscrowValidatorScript = compileScript auctionEscrowValidatorUntyped

----------------------------------------------------------------------
-- StandingBid

standingBidValidatorUntyped
  :: ClosedTerm
      ( PAsData PCurrencySymbol
          :--> PAsData PAuctionTerms
          :--> PValidator
      )
standingBidValidatorUntyped = phoistAcyclic $
  plam $ \auctionCs auctionTerms datum redeemer ctx ->
    popaque $
      standingBidValidator
        # pfromData auctionCs
        # pfromData auctionTerms
        # punsafeCoerce datum
        # punsafeCoerce redeemer
        # ctx

standingBidValidatorScript :: Script
standingBidValidatorScript = compileScript standingBidValidatorUntyped

----------------------------------------------------------------------
-- BidderDeposit

bidderDepositValidatorUntyped
  :: ClosedTerm
      ( PAsData PStandingBidScriptHash
          :--> PAsData PAuctionEscrowScriptHash
          :--> PAsData PCurrencySymbol
          :--> PAsData PAuctionTerms
          :--> PValidator
      )
bidderDepositValidatorUntyped = phoistAcyclic $
  plam $ \standingBidSh auctionEscrowSh auctionCs auctionTerms datum redeemer ctx ->
    popaque $
      bidderDepositValidator
        # pfromData standingBidSh
        # pfromData auctionEscrowSh
        # pfromData auctionCs
        # pfromData auctionTerms
        # punsafeCoerce datum
        # punsafeCoerce redeemer
        # ctx

bidderDepositValidatorScript :: Script
bidderDepositValidatorScript = compileScript bidderDepositValidatorUntyped

----------------------------------------------------------------------
-- AuctionMetadata

auctionMetadataValidatorUntyped :: ClosedTerm PValidator
auctionMetadataValidatorUntyped = phoistAcyclic $
  plam $ \datum redeemer ctx ->
    popaque $
      auctionMetadataValidator
        # punsafeCoerce datum
        # punsafeCoerce redeemer
        # ctx

auctionMetadataValidatorScript :: Script
auctionMetadataValidatorScript = compileScript auctionMetadataValidatorUntyped

--

config :: Config
config = Config DoTracing

writeScript :: TypedWriter a => Text -> FilePath -> ClosedTerm a -> IO ()
writeScript desc term = writeTypedScript config desc term

compileScript :: ClosedTerm a -> Script
compileScript term =
  either (error . mappend "Plutarch compilation error: " . T.unpack) id $
    compile config term
