.PHONY: build, repl, format, hoogle, auction_metadata_validator

hs-sources := $(shell fd --no-ignore-parent -ehs)
cabal-sources := $(shell fd --no-ignore-parent -ecabal)

build:
	cabal v2-build all

repl:
	cabal v2-repl hydra-auction-onchain --ghc-options '-Wno-missing-import-lists'

format:
	fourmolu -m inplace ${hs-sources} && cabal-fmt -i ${cabal-sources} && nix run '.#nixFormat'

hoogle:
	hoogle server --local --port=8070 > /dev/null &

auction_metadata_validator:
	cabal v2-run hydra-auction-onchain-exe
