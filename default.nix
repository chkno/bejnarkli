{ pkgs ? import <nixpkgs> { } }:
pkgs.callPackage ({ lib, mkDerivation, stdenv, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    license = lib.licenses.mit;
  }) { inherit (pkgs.haskellPackages) mkDerivation; }
