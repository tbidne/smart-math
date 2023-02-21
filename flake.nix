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
      url = "github:tbidne/algebra-simple";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
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
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            (hlib.dontCheck ghcid)
            (hlib.overrideCabal haskell-language-server (old: {
              configureFlags = (old.configureFlags or [ ]) ++
                [
                  "-f -brittany"
                  "-f -floskell"
                  "-f -fourmolu"
                  "-f -stylishhaskell"
                ];
            }))
          ];
          ghc-version = "ghc944";
          compiler = pkgs.haskell.packages."${ghc-version}";
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "smart-math";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if returnShellEnv then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                algebra-simple =
                  final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds =
                  final.callCabal2nix "bounds" bounds { };
              };
            };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
