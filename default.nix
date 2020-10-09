{ pkgs ? import <nixpkgs> { } }:
pkgs.haskellPackages.callPackage ({ lib, mkDerivation, QuickCheck, stdenv, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    testHaskellDepends = [ QuickCheck ];
    license = lib.licenses.mit;
  }) { }
