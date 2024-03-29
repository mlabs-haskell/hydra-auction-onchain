module Main (main) where

import HydraAuctionOnchain.Scripts
  ( auctionEscrowValidatorUntyped
  , auctionMetadataValidatorUntyped
  , auctionMintingPolicyUntyped
  , bidderDepositValidatorUntyped
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
      writeAuctionMintingPolicy
      writeAuctionEscrowValidator
      writeStandingBidValidator
      writeBidderDepositValidator
      writeAuctionMetadataValidator
    AuctionMintingPolicy ->
      writeAuctionMintingPolicy
    AuctionEscrowValidator ->
      writeAuctionEscrowValidator
    StandingBidValidator ->
      writeStandingBidValidator
    BidderDepositValidator ->
      writeBidderDepositValidator
    AuctionMetadataValidator ->
      writeAuctionMetadataValidator

writeAuctionMintingPolicy :: IO ()
writeAuctionMintingPolicy =
  writeScript
    "Auction minting policy"
    "compiled/auction_minting_policy.plutus"
    auctionMintingPolicyUntyped

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

writeBidderDepositValidator :: IO ()
writeBidderDepositValidator =
  writeScript
    "Bidder deposit validator"
    "compiled/bidder_deposit_validator.plutus"
    bidderDepositValidatorUntyped

writeAuctionMetadataValidator :: IO ()
writeAuctionMetadataValidator =
  writeScript
    "Auction metadata validator"
    "compiled/auction_metadata_validator.plutus"
    auctionMetadataValidatorUntyped

data ScriptToCompile
  = AllScripts
  | AuctionMintingPolicy
  | AuctionEscrowValidator
  | StandingBidValidator
  | BidderDepositValidator
  | AuctionMetadataValidator
  deriving stock (Show, Eq)

toScript :: String -> Maybe ScriptToCompile
toScript = \case
  "all" -> Just AllScripts
  "auction_mp" -> Just AuctionMintingPolicy
  "auction_escrow" -> Just AuctionEscrowValidator
  "standing_bid" -> Just StandingBidValidator
  "bidder_deposit" -> Just BidderDepositValidator
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
