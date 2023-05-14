{
  description = "A package for mathematical smart constructors";
  inputs = {
    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils = {
      url = "github:tbidne/nix-hs-utils";
      inputs.flake-compat.follows = "flake-compat";
    };

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple/";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds/";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{ flake-compat
    , flake-parts
    , nix-hs-utils
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc944";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              apply-refact = prev.apply-refact_0_11_0_0;
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
            ];
          };
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "smart-math";
              root = ./.;
            };
          hs-dirs = "src test";
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hs-dirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hs-dirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hs-dirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
