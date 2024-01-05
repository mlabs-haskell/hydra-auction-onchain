module HydraAuctionOnchain.Types.AuctionEscrowState
  ( PAuctionEscrowState (AuctionAnnounced, BiddingStarted, AuctionConcluded)
  , pvalidateAuctionEscrowTransitionToAuctionConcluded
  , pvalidateAuctionEscrowTransitionToBiddingStarted
  ) where

data PAuctionEscrowState (s :: S)
  = AuctionAnnounced (Term s (PDataRecord '[]))
  | BiddingStarted (Term s (PDataRecord '[]))
  | AuctionConcluded (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow, PEq)

instance DerivePlutusType PAuctionEscrowState where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PAuctionEscrowState

pvalidateAuctionEscrowTransitionToBiddingStarted
  :: Term
      s
      ( PAuctionEscrowState
          :--> PAuctionEscrowState
          :--> PBool
      )
pvalidateAuctionEscrowTransitionToBiddingStarted =
  phoistAcyclic $
    plam $ \oldState newState ->
      (oldState #== pcon (AuctionAnnounced pdnil))
        #&& (newState #== pcon (BiddingStarted pdnil))

pvalidateAuctionEscrowTransitionToAuctionConcluded
  :: Term
      s
      ( PAuctionEscrowState
          :--> PAuctionEscrowState
          :--> PBool
      )
pvalidateAuctionEscrowTransitionToAuctionConcluded =
  phoistAcyclic $
    plam $ \oldState newState ->
      (oldState #== pcon (BiddingStarted pdnil))
        #&& (newState #== pcon (AuctionConcluded pdnil))
