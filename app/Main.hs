module Main (main) where

import HydraAuctionOnchain.Scripts
  ( auctionEscrowValidatorUntyped
  , auctionMetadataValidatorUntyped
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
      writeAuctionEscrowValidator
      writeStandingBidValidator
      writeAuctionMetadataValidator
    AuctionEscrowValidator ->
      writeAuctionEscrowValidator
    StandingBidValidator ->
      writeStandingBidValidator
    AuctionMetadataValidator ->
      writeAuctionMetadataValidator

writeAuctionEscrowValidator :: IO ()
writeAuctionEscrowValidator =
  writeScript
    "Auction escrow validator"
    "compiled/auction_escrow_validator.plutus"
    auctionEscrowValidatorUntyped

writeStandingBidValidator :: IO ()
writeStandingBidValidator =
  writeScript
    "Standing bid validator"
    "compiled/standing_bid_validator.plutus"
    standingBidValidatorUntyped

writeAuctionMetadataValidator :: IO ()
writeAuctionMetadataValidator =
  writeScript
    "Auction metadata validator"
    "compiled/auction_metadata_validator.plutus"
    auctionMetadataValidatorUntyped

data ScriptToCompile
  = AllScripts
  | AuctionEscrowValidator
  | StandingBidValidator
  | AuctionMetadataValidator
  deriving stock (Show, Eq)

toScript :: String -> Maybe ScriptToCompile
toScript = \case
  "all" -> Just AllScripts
  "auction_escrow" -> Just AuctionEscrowValidator
  "standing_bid" -> Just StandingBidValidator
  "metadata" -> Just AuctionMetadataValidator
  _ -> Nothing

scriptToCompile :: Parser ScriptToCompile
scriptToCompile =
  option
    (maybeReader toScript)
    ( long "script"
        <> metavar "SCRIPT"
        <> help "Name of the Plutarch script to compile"
    )
