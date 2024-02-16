.PHONY: build test repl format hoogle all_scripts auction_escrow_validator standing_bid_validator bidder_deposit_validator auction_metadata_validator

hs-sources := $(shell fd --no-ignore-parent -ehs)
cabal-sources := $(shell fd --no-ignore-parent -ecabal)

build:
	cabal v2-build all

test:
	cabal v2-test --test-show-details=streaming --test-options="--quickcheck-tests=100"

repl:
	cabal v2-repl hydra-auction-onchain --ghc-options '-Wno-missing-import-lists'

format:
	fourmolu -m inplace ${hs-sources} && cabal-fmt -i ${cabal-sources} && nix run '.#nixFormat'

hoogle:
	hoogle server --local --port=8070 > /dev/null &

all_scripts:
	cabal v2-run hydra-auction-onchain-exe -- --script all

auction_escrow_validator:
	cabal v2-run hydra-auction-onchain-exe -- --script auction_escrow

standing_bid_validator:
	cabal v2-run hydra-auction-onchain-exe -- --script standing_bid

bidder_deposit_validator:
	cabal v2-run hydra-auction-onchain-exe -- --script bidder_deposit

auction_metadata_validator:
	cabal v2-run hydra-auction-onchain-exe -- --script metadata
