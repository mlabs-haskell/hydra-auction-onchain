module Main (main) where

import HydraAuctionOnchain.Scripts
  ( auctionMetadataValidatorUntyped
  , standingBidValidatorUntyped
  , writeScript
  )
import Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , maybeReader
  , metavar
  , option
  )

main :: IO ()
main =
  execParser (info (helper <*> scriptToCompile) fullDesc) >>= \case
    AllScripts -> do
      writeAuctionMetadataValidator
      writeStandingBidValidator
    AuctionMetadataValidator ->
      writeAuctionMetadataValidator
    StandingBidValidator ->
      writeStandingBidValidator

writeAuctionMetadataValidator :: IO ()
writeAuctionMetadataValidator =
  writeScript
    "Auction metadata validator"
    "compiled/auction_metadata_validator.plutus"
    auctionMetadataValidatorUntyped

writeStandingBidValidator :: IO ()
writeStandingBidValidator =
  writeScript
    "Standing bid validator"
    "compiled/standing_bid_validator.plutus"
    standingBidValidatorUntyped

data ScriptToCompile
  = AllScripts
  | AuctionMetadataValidator
  | StandingBidValidator
  deriving stock (Show, Eq)

toScript :: String -> Maybe ScriptToCompile
toScript = \case
  "all" -> Just AllScripts
  "metadata" -> Just AuctionMetadataValidator
  "standing_bid" -> Just StandingBidValidator
  _ -> Nothing

scriptToCompile :: Parser ScriptToCompile
scriptToCompile =
  option
    (maybeReader toScript)
    ( long "script"
        <> metavar "SCRIPT"
        <> help "Name of the Plutarch script to compile"
    )
