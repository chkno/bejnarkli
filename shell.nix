{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
    zlib

    # keep this line if you use bash
    bashInteractive
  ];
}
