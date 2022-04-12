{
  description = "A package for mathematical smart constructors";
  inputs.algebra-simple-src.url = "github:tbidne/algebra-simple/no-tfs";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    { algebra-simple-src
    , flake-utils
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc921";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "smart-math";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              ghcid
              ormolu
              pkgs.zlib
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
