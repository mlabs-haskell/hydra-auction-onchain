module HydraAuctionOnchain.Scripts
  ( auctionEscrowValidatorScript
  , auctionEscrowValidatorUntyped
  , auctionMetadataValidatorScript
  , auctionMetadataValidatorUntyped
  , compileScript
  , standingBidValidatorScript
  , standingBidValidatorUntyped
  , writeScript
  ) where

import Data.Text (Text)
import Data.Text qualified as T (unpack)
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms)
import HydraAuctionOnchain.Validators.AuctionEscrow (auctionEscrowValidator)
import HydraAuctionOnchain.Validators.AuctionMetadata (auctionMetadataValidator)
import HydraAuctionOnchain.Validators.StandingBid (standingBidValidator)
import Plutarch (Config (Config), Script, TracingMode (DoTracingAndBinds), compile)
import Plutarch.Api.V2 (PCurrencySymbol, PScriptHash, PValidator)
import Plutarch.Unsafe (punsafeCoerce)
import Ply.Plutarch.TypedWriter (TypedWriter, writeTypedScript)

--------------------------------------------------------------------------------
-- AuctionEscrow
--------------------------------------------------------------------------------

auctionEscrowValidatorUntyped
  :: ClosedTerm
      ( PAsData PScriptHash
          :--> PAsData PScriptHash
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

--------------------------------------------------------------------------------
-- StandingBid
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- AuctionMetadata
--------------------------------------------------------------------------------

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
config = Config DoTracingAndBinds

writeScript :: TypedWriter a => Text -> FilePath -> ClosedTerm a -> IO ()
writeScript desc term = writeTypedScript config desc term

compileScript :: ClosedTerm a -> Script
compileScript term =
  either (error . mappend "Plutarch compilation error: " . T.unpack) id $
    compile config term
