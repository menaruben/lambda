{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = [
    # lambda interpreter
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server

    # tree-sitter grammar
    pkgs.tree-sitter
    pkgs.nodejs
  ];
}
