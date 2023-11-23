.PHONY: build, repl, format

hs-sources := $(shell fd --no-ignore-parent -ehs)
cabal-sources := $(shell fd --no-ignore-parent -ecabal)

build:
	cabal v2-build all

repl:
	cabal v2-repl hydra-auction-onchain 

format:
	fourmolu -m inplace ${hs-sources} && cabal-fmt -i ${cabal-sources} && nix run '.#nixFormat'
