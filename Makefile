.PHONY: build test repl format hoogle all_scripts auction_metadata_validator standing_bid_validator

hs-sources := $(shell fd --no-ignore-parent -ehs)
cabal-sources := $(shell fd --no-ignore-parent -ecabal)

build:
	cabal v2-build all

test:
	cabal v2-test

repl:
	cabal v2-repl hydra-auction-onchain --ghc-options '-Wno-missing-import-lists'

format:
	fourmolu -m inplace ${hs-sources} && cabal-fmt -i ${cabal-sources} && nix run '.#nixFormat'

hoogle:
	hoogle server --local --port=8070 > /dev/null &

all_scripts:
	cabal v2-run hydra-auction-onchain-exe -- --script all

auction_metadata_validator:
	cabal v2-run hydra-auction-onchain-exe -- --script metadata

standing_bid_validator:
	cabal v2-run hydra-auction-onchain-exe -- --script standing_bid
