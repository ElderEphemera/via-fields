{ pkgs ? import <nixpkgs> {}
, ghc ? "9_2_1-rc1"
}:

let
  ghcs = import (pkgs.fetchgit {
    url = "https://gitlab.haskell.org/bgamari/ghcs-nix";
    rev = "ec58e1bcbaf8b35fb352efdf121106d100181d9d";
    sha256 = "17q987vrm026xxya779zmqpril5i8dl654wl1ckln8xkqc8rqx2i";
  }) {};
in pkgs.mkShell {
  packages = [
    ghcs."ghc-${ghc}"
    ghcs.cabal-install
  ];
}
