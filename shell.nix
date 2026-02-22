{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}
