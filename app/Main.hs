module Main (main) where

import HydraAuctionOnchain.Scripts (auctionMetadataValidatorUntyped, writeScript)

main :: IO ()
main =
  writeScript
    "Auction metadata validator"
    "compiled/auction_metadata_validator.plutus"
    auctionMetadataValidatorUntyped
