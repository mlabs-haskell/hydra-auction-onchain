module Spec.HydraAuctionOnchain.Validators.StandingBid (spec) where

import HydraAuctionOnchain.Scripts (compileScript)
import HydraAuctionOnchain.Validators.StandingBid (standingBidValidator)
import Plutarch (Script)
import PlutusLedgerApi.V2 (CurrencySymbol, ScriptContext)
import Spec.HydraAuctionOnchain.Helpers (shouldFail)
import Spec.HydraAuctionOnchain.Types.AuctionTerms (AuctionTerms)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)

spec :: TestTree
spec =
  testGroup
    "StandingBid"
    [ testProperty "Fails when own input is missing" prop_ownInputMissing_fails
    ]

prop_ownInputMissing_fails :: Property
prop_ownInputMissing_fails = shouldFail undefined

compile
  :: CurrencySymbol
  -> AuctionTerms
  -> StandingBidState
  -> StandingBidRedeemer
  -> ScriptContext
  -> Script
compile auctionCs auctionTerms datum redeemer =
  compileScript $
    popaque $
      standingBidValidator
        # pconstant auctionCs
        # pconstant auctionTerms
        # pconstant datum
        # pconstant redeemer
