module Main (main) where

import Spec.HydraAuctionOnchain.Validators.StandingBid qualified as StandingBid (spec)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "hydra-auction-onchain"
      [ StandingBid.spec
      ]
