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
    inputs@{ algebra-simple
    , bounds
    , flake-compat
    , flake-parts
    , nixpkgs
    , self
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: [
            c.cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: [
            (hlib.dontCheck c.apply-refact)
            (hlib.dontCheck c.cabal-fmt)
            (hlib.dontCheck c.haskell-language-server)
            (hlib.dontCheck c.hlint)
            (hlib.dontCheck c.ormolu)
            pkgs.nixpkgs-fmt
          ];
          ghc-version = "ghc944";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
              apply-refact = prev.apply-refact_0_11_0_0;
              bounds = final.callCabal2nix "bounds" bounds { };
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          mkPkg = returnShellEnv:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "smart-math";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if returnShellEnv then devTools compiler else [ ]));
            };
          mkApp = drv: {
            type = "app";
            program = "${drv}/bin/${drv.name}";
          };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = mkApp (
              pkgs.writeShellApplication {
                name = "format";
                text = builtins.readFile ./tools/format.sh;
                runtimeInputs = [
                  compiler.cabal-fmt
                  compiler.ormolu
                  pkgs.nixpkgs-fmt
                ];
              }
            );
            lint = mkApp (
              pkgs.writeShellApplication {
                name = "lint";
                text = builtins.readFile ./tools/lint.sh;
                runtimeInputs = [ compiler.hlint ];
              }
            );
            lint-refactor = mkApp (
              pkgs.writeShellApplication {
                name = "lint-refactor";
                text = builtins.readFile ./tools/lint-refactor.sh;
                runtimeInputs = [ compiler.apply-refact compiler.hlint ];
              }
            );
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
