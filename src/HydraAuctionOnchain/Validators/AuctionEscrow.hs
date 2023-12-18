module HydraAuctionOnchain.Validators.AuctionEscrow
  ( pisConcluding
  ) where

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
