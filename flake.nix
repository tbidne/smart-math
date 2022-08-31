{
  description = "A package for mathematical smart constructors";
  inputs.algebra-simple-src.url = "github:tbidne/algebra-simple";
  inputs.bounds-src.url = "github:tbidne/bounds";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    { algebra-simple-src
    , bounds-src
    , flake-compat
    , flake-utils
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      buildTools = c: with c; [
        cabal-install
        pkgs.gnumake
        pkgs.zlib
      ];
      devTools = c: with c; [
        ghcid
        haskell-language-server
      ];
      ghc-version = "ghc923";
      compiler = pkgs.haskell.packages."${ghc-version}";
      mkPkg = returnShellEnv: withDevTools:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "smart-math";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (buildTools compiler ++
                (if withDevTools then devTools compiler else [ ]));
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            bounds =
              final.callCabal2nix "bounds" bounds-src { };
          };
        };
    in
    {
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
