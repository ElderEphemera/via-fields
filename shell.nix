{ pkgs ? import <nixpkgs> {}
, ghc ? "9_2_1-rc1"
}:

let
  ghcs = import (pkgs.fetchgit {
    url = "https://gitlab.haskell.org/bgamari/ghcs-nix";
    rev = "ec709325f95b6413cdad9437b8f58a3dff74610f";
    sha256 = "1ydfsi4riyml5jk66j58q2jixxkjnid0xgwv5nyfxd2xb64g1y72";
  }) {};
in pkgs.mkShell {
  packages = [
    ghcs."ghc-${ghc}"
    ghcs.cabal-install
  ];
}
