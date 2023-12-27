module HydraAuctionOnchain.Validators.AuctionMetadata
  ( auctionMetadataValidator
  ) where

import HydraAuctionOnchain.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , standingBidTokenName
  )
import HydraAuctionOnchain.Types.AuctionInfo (PAuctionInfo)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Extra.Bool (passert)
import Plutarch.Extra.ScriptContext (ptryOwnInput)
import Plutarch.Monadic qualified as P

data PAuctionMetadataRedeemer (s :: S)
  = RemoveAuctionRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PAuctionMetadataRedeemer where
  type DPTStrat _ = PlutusTypeData

auctionMetadataValidator
  :: Term
      s
      ( PAuctionInfo
          :--> PAuctionMetadataRedeemer
          :--> PScriptContext
          :--> PUnit
      )
auctionMetadataValidator = phoistAcyclic $
  plam $ \auctionInfo redeemer ctx ->
    pmatch redeemer $ \case
      RemoveAuctionRedeemer _ -> P.do
        auctionCurrencySymbol <- plet $ pfield @"auctionId" # auctionInfo

        -- 1. Check that the target utxo contains exactly one auction metadata token:
        utxoValue <- plet $ pfield @"value" #$ pfield @"resolved" #$ ptryOwnInput # ctx
        passert "Target utxo must contain one auction metadata token" $
          (pvalueOf # utxoValue # auctionCurrencySymbol # auctionMetadataTokenName) #== 1

        -- 2. Check that the transaction burns auction token bundle:
        mintValue <- plet $ pfield @"mint" #$ pfield @"txInfo" # ctx
        burnsOneAuctionToken <-
          plet $ plam $ \tn ->
            (pvalueOf # mintValue # auctionCurrencySymbol # tn) #== -1

        passert "Transaction must burn auction token bundle" $
          (burnsOneAuctionToken # auctionEscrowTokenName)
            #&& (burnsOneAuctionToken # auctionMetadataTokenName)
            #&& (burnsOneAuctionToken # standingBidTokenName)

        pcon PUnit
