module Spec.HydraAuctionOnchain.Helpers
  ( shouldFail
  ) where

import Data.Text.Lazy qualified as TL (unpack)
import Plutarch (Script)
import Plutarch.Evaluate (evalScript)
import Test.Tasty.QuickCheck (Property, counterexample, property)
import Text.Pretty.Simple (pShow)

shouldFail :: Script -> Property
shouldFail script =
  case result of
    Left _ -> property True
    Right _ ->
      counterexample "Expected failure, but succeeded instead."
        . counterexample (showLogs logs)
        . property
        $ False
  where
    (result, _exUnits, logs) = evalScript script

showLogs :: Show a => [a] -> String
showLogs = \case
  [] -> "No logs found. Did you forget to compile with tracing on?"
  logs -> TL.unpack $ pShow logs
