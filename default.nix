{ pkgs ? import <nixpkgs> { } }:
pkgs.haskellPackages.callPackage
({ lib, mkDerivation, QuickCheck, quickcheck-instances, stdenv, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    testHaskellDepends = [ QuickCheck quickcheck-instances ];
    license = lib.licenses.mit;
  }) { }
