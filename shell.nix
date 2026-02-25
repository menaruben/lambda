{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = [
    # interpreter
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server

    # custom syntax highlighting of lambda calculus grammar
    pkgs.tree-sitter
    pkgs.nodejs

    # utf8 encoding
    pkgs.glibcLocales
  ];

  shellHook = ''
    export LANG=en_US.UTF-8
    export LC_ALL=en_US.UTF-8
  '';
}
