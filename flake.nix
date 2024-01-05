{
  description = "A nix flake for hydra-auction-onchain.";

  nixConfig = {
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction-onchain@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    extra-substituters = [
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
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
      herculesCI.ciSystems = [ "x86_64-linux" ];
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
