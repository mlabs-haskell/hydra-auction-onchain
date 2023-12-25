module Spec.HydraAuctionOnchain.Expectations
  ( shouldFail
  , shouldFailWithError
  , shouldSucceed
  ) where

import Data.Text qualified as T (unpack)
import Data.Text.Lazy qualified as TL (unpack)
import HydraAuctionOnchain.Types.Error (ErrorCode (toErrorCode))
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

shouldFailWithError :: ErrorCode e => e -> Script -> Property
shouldFailWithError errExpected script =
  case result of
    Left _ | last logs == errCode -> property True
    Left _ ->
      counterexample ("Expected failure with error code " <> T.unpack errCode <> ".")
        . counterexample (showLogs logs)
        . property
        $ False
    Right _ ->
      counterexample "Expected failure, but succeeded instead."
        . counterexample (showLogs logs)
        . property
        $ False
  where
    errCode = toErrorCode errExpected
    (result, _exUnits, logs) = evalScript script

shouldSucceed :: Script -> Property
shouldSucceed script =
  case result of
    Left err ->
      counterexample "Expected success, but failed instead."
        . counterexample ("Error: " <> show err)
        . counterexample (showLogs logs)
        . property
        $ False
    Right _ -> property True
  where
    (result, _exUnits, logs) = evalScript script

showLogs :: Show a => [a] -> String
showLogs = \case
  [] -> "No logs found. Did you forget to compile with tracing on?"
  logs -> TL.unpack $ pShow logs
