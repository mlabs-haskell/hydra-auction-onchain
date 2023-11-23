{
  description = "A nix flake for hydra-auction-onchain.";

  nixConfig = {
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction-onchain@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";
    liqwid-libs.url = "github:Liqwid-Labs/liqwid-libs";
    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix/v2.9.2";
    liqwid-nix.inputs.nixpkgs-latest.follows = "nixpkgs-latest";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest { inherit system; };
        in
        {
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskell.packages.ghc943.fourmolu;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = { };
            hoogleImage.enable = false;
            enableBuildChecks = true;
            extraHackageDeps = [
              "${inputs.liqwid-libs}/liqwid-plutarch-extra"
              "${inputs.liqwid-libs}/plutarch-quickcheck"
              "${inputs.liqwid-libs.inputs.ply}/ply-core"
              "${inputs.liqwid-libs.inputs.ply}/ply-plutarch"
            ];
          };
        };
    };
}
