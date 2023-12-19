{-# LANGUAGE TemplateHaskell #-}

module HydraAuctionOnchain.Validators.AuctionEscrow
  ( auctionEscrowValidator
  , pisConcluding
  ) where

import HydraAuctionOnchain.Errors.AuctionEscrow (PAuctionEscrowError (..))
import HydraAuctionOnchain.Helpers
  ( pdecodeInlineDatum
  , pfindUniqueOutputWithAddress
  , pfindUniqueOutputWithScriptHash
  , pgetOwnInput
  , ponlyOneInputFromAddress
  , ptxOutContainsAuctionEscrowToken
  , putxoAddress
  )
import HydraAuctionOnchain.Types.AuctionEscrowState
  ( PAuctionEscrowState
  , pvalidateAuctionEscrowTransitionToBiddingStarted
  )
import HydraAuctionOnchain.Types.AuctionTerms (PAuctionTerms, pbiddingPeriod)
import HydraAuctionOnchain.Types.Error (errCode, passert, passertMaybe)
import HydraAuctionOnchain.Types.StandingBidState (PStandingBidState (PStandingBidState))
import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.Maybe (pdnothing)
import Plutarch.Extra.ScriptContext (ptxSignedBy)
import Plutarch.Monadic qualified as P

--------------------------------------------------------------------------------
-- Redeemers
--------------------------------------------------------------------------------

data PAuctionEscrowRedeemer (s :: S)
  = StartBiddingRedeemer (Term s (PDataRecord '[]))
  | BidderBuysRedeemer (Term s (PDataRecord '[]))
  | SellerReclaimsRedeemer (Term s (PDataRecord '[]))
  | CleanupAuctionRedeemer (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PAuctionEscrowRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PAuctionEscrowRedeemer)

pisConcluding :: Term s (PAuctionEscrowRedeemer :--> PBool)
pisConcluding = phoistAcyclic $
  plam $ \redeemer -> pmatch redeemer $ \case
    BidderBuysRedeemer _ -> pcon PTrue
    SellerReclaimsRedeemer _ -> pcon PTrue
    _ -> pcon PFalse

--------------------------------------------------------------------------------
-- Validator
--------------------------------------------------------------------------------

auctionEscrowValidator
  :: Term
      s
      ( PScriptHash
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PAuctionEscrowRedeemer
          :--> PScriptContext
          :--> PUnit
      )
auctionEscrowValidator = phoistAcyclic $
  plam $ \sbScriptHash auctionCs auctionTerms oldAuctionState redeemer ctx -> P.do
    txInfo <- plet $ pfield @"txInfo" # ctx

    -- (AUES0) The validator's own input should exist.
    ownInput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'Error'MissingAuctionEscrowInput)
          (pgetOwnInput # ctx)

    -- (AUES1) There should only be one auction escrow input.
    ownAddress <- plet $ putxoAddress # ownInput
    passert $(errCode AuctionEscrow'Error'TooManyOwnScriptInputs) $
      ponlyOneInputFromAddress # ownAddress # txInfo

    -- (AUES2) The auction escrow input should contain an auction
    -- escrow token.
    passert $(errCode AuctionEscrow'Error'OwnInputMissingToken) $
      ptxOutContainsAuctionEscrowToken # auctionCs #$ pfield @"resolved" # ownInput

    -- Branching checks based on the redeemer used.
    pmatch redeemer $ \case
      StartBiddingRedeemer _ ->
        pcheckStartBidding
          # sbScriptHash
          # txInfo
          # auctionCs
          # auctionTerms
          # oldAuctionState
          # ownAddress
      BidderBuysRedeemer _ ->
        undefined
      SellerReclaimsRedeemer _ ->
        undefined
      CleanupAuctionRedeemer _ ->
        undefined

--------------------------------------------------------------------------------
-- StartBidding
--------------------------------------------------------------------------------

pcheckStartBidding
  :: Term
      s
      ( PScriptHash
          :--> PTxInfo
          :--> PCurrencySymbol
          :--> PAuctionTerms
          :--> PAuctionEscrowState
          :--> PAddress
          :--> PUnit
      )
pcheckStartBidding = phoistAcyclic $
  plam $ \sbScriptHash txInfo auctionCs auctionTerms oldAuctionState ownAddress -> P.do
    txInfoFields <- pletFields @["signatories", "validRange"] txInfo

    -- (AUES3) There should be no tokens minted or burned.
    mintValue <- plet $ pfield @"mint" # txInfo
    passert $(errCode AuctionEscrow'StartBidding'Error'UnexpectedTokensMintedBurned) $
      pfromData mintValue #== mempty

    -- (AUES4) This redeemer can only be used during
    -- the bidding period.
    passert $(errCode AuctionEscrow'StartBidding'Error'IncorrectValidityInterval) $
      pcontains # (pbiddingPeriod # auctionTerms) # txInfoFields.validRange

    -- (AUES5) The transaction should be signed by the seller.
    passert $(errCode AuctionEscrow'StartBidding'Error'MissingSellerSignature) $
      ptxSignedBy
        # txInfoFields.signatories
        # (pfield @"sellerPkh" # auctionTerms)

    ----------------------------------------------------------------------------
    -- Check auction escrow state transition
    ----------------------------------------------------------------------------

    -- (AUES6) There should be exactly one auction escrow output.
    ownOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'MissingAuctionEscrowOutput)
          (pfindUniqueOutputWithAddress # ownAddress # txInfo)

    -- (AUES7) The auction escrow output should contain an auction
    -- escrow token.
    passert $(errCode AuctionEscrow'StartBidding'Error'AuctionEscrowOutputMissingToken) $
      ptxOutContainsAuctionEscrowToken # auctionCs # ownOutput

    -- (AUES8) The auction escrow output's datum should be decodable
    -- as an auction escrow state.
    newAuctionState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'FailedToDecodeAuctionEscrowState)
          (pdecodeInlineDatum # ownOutput)

    -- (AUES9) The auction state should transition from
    -- `AuctionAnnounced` to `BiddingStarted`.
    passert $(errCode AuctionEscrow'StartBidding'Error'InvalidAuctionStateTransition) $
      pvalidateAuctionEscrowTransitionToBiddingStarted
        # oldAuctionState
        # newAuctionState

    ----------------------------------------------------------------------------
    -- Check standing bid state
    ----------------------------------------------------------------------------

    -- (AUES10) There should be exactly one standing bid output.
    standingBidOutput <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'MissingStandingBidOutput)
          (pfindUniqueOutputWithScriptHash # sbScriptHash # txInfo)

    -- (AUES11) The standing bid output's datum should be decodable
    -- as a standing bid state.
    initialBidState <-
      plet $
        passertMaybe
          $(errCode AuctionEscrow'StartBidding'Error'FailedToDecodeStandingBidState)
          (pdecodeInlineDatum # standingBidOutput)

    -- (AUES12) The standing bid state should be initialized
    -- without bid terms.
    passert $(errCode AuctionEscrow'StartBidding'Error'InvalidStandingBidState) $
      initialBidState #== pcon (PStandingBidState pdnothing)

    undefined
