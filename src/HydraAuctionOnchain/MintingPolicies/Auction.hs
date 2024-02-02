{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.MintingPolicies.Auction
  ( auctionMintingPolicy
  ) where

import HydraAuctionOnchain.Errors.MintingPolicies.Auction (PAuctionMpError (..))
import HydraAuctionOnchain.Helpers (pdecodeInlineDatum, pfindUniqueOutputWithScriptHash)
import HydraAuctionOnchain.Lib.ScriptContext (pownCurrencySymbol)
import HydraAuctionOnchain.Types.AuctionInfo (PAuctionInfo)
import HydraAuctionOnchain.Types.AuctionTerms (pvalidateAuctionTerms)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.Tokens
  ( pauctionTokenBundleBurned
  , pauctionTokenBundleMinted
  , ptxOutContainsAuctionMetadataToken
  )
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PScriptHash, PTxOutRef)
import Plutarch.Extra.Maybe (pisJust)
import Plutarch.Extra.ScriptContext (pfindTxInByTxOutRef)
import Plutarch.Monadic qualified as P

data PAuctionMpRedeemer (s :: S)
  = MintAuctionRedeemer (Term s (PDataRecord '[]))
  | BurnAuctionRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PAuctionMpRedeemer where
  type DPTStrat _ = PlutusTypeData

auctionMintingPolicy
  :: Term
      s
      ( PScriptHash
          :--> PTxOutRef
          :--> PAuctionMpRedeemer
          :--> PScriptContext
          :--> PUnit
      )
auctionMintingPolicy = phoistAcyclic $
  plam $ \auctionMetadataSh nonceOref redeemer ctx -> P.do
    -- Script purpose should contain own currency symbol. Always
    -- true for minting policies.
    auctionCs <-
      plet $
        passertMaybe
          $(errCode AuctionMp'Error'MissingOwnCurrencySymbol)
          (pownCurrencySymbol # ctx)

    pmatch redeemer $ \case
      MintAuctionRedeemer _ ->
        pcheckMintAuction # auctionMetadataSh # nonceOref # ctx # auctionCs
      BurnAuctionRedeemer _ ->
        pcheckBurnAuction # ctx # auctionCs

pcheckMintAuction
  :: Term
      s
      ( PScriptHash
          :--> PTxOutRef
          :--> PScriptContext
          :--> PCurrencySymbol
          :--> PUnit
      )
pcheckMintAuction = phoistAcyclic $
  plam $ \auctionMetadataSh nonceOref ctx auctionCs -> P.do
    txInfo <- plet $ pfield @"txInfo" # ctx
    txInfoFields <- pletFields @["mint", "inputs"] txInfo
    mintValue <- plet $ pnormalize # txInfoFields.mint
    let inputs = txInfoFields.inputs

    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be minted.
    -- No other tokens should be minted or burned.
    passert $(errCode AuctionMp'MintAuction'Error'AuctionTokenBundleNotMinted) $
      mintValue #== pauctionTokenBundleMinted # auctionCs

    -- The utxo nonce parameter of the minting policy should be a
    -- reference to a utxo input spent by the transaction.
    passert $(errCode AuctionMp'MintAuction'Error'MissingUtxoNonceInput) $
      pisJust #$ pfindTxInByTxOutRef # nonceOref # inputs

    -- There should be exactly one auction metadata output.
    auctionMetadataOutput <-
      plet $
        passertMaybe
          $(errCode AuctionMp'MintAuction'Error'MissingAuctionMetadataOutput)
          (pfindUniqueOutputWithScriptHash # auctionMetadataSh # txInfo)

    -- The auction metadata output should contain an auction
    -- metadata token.
    passert $(errCode AuctionMp'MintAuction'Error'AuctionMetadataOutputMissingToken) $
      ptxOutContainsAuctionMetadataToken # auctionCs # auctionMetadataOutput

    -- The auction metadata output contains a datum that can be
    -- decoded as an auction info metadata record.
    (auctionInfo :: Term s PAuctionInfo) <-
      plet $
        passertMaybe
          $(errCode AuctionMp'MintAuction'Error'FailedToDecodeAuctionInfo)
          (pdecodeInlineDatum # auctionMetadataOutput)

    -- The auction info metadata record should contain an auction ID
    -- that matches this minting policy's currency symbol.
    auctionInfoFields <- pletFields @["auctionId", "auctionTerms"] auctionInfo
    passert $(errCode AuctionMp'MintAuction'Error'AuctionInfoCurrencySymbolMismatch) $
      auctionInfoFields.auctionId #== auctionCs

    -- The auction metadata record should contain valid
    -- auction terms.
    pvalidateAuctionTerms # auctionInfoFields.auctionTerms

pcheckBurnAuction :: Term s (PScriptContext :--> PCurrencySymbol :--> PUnit)
pcheckBurnAuction = phoistAcyclic $
  plam $ \ctx auctionCs -> P.do
    mintValue <- plet $ pnormalize #$ pfield @"mint" #$ pfield @"txInfo" # ctx

    -- The auction state, auction metadata, and standing bid tokens
    -- of the auction should all be burned.
    -- No other tokens should be minted or burned.
    passert $(errCode AuctionMp'BurnAuction'Error'AuctionTokenBundleNotBurned) $
      mintValue #== pauctionTokenBundleBurned # auctionCs

    pcon PUnit
