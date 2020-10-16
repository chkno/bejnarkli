{ pkgs ? import <nixpkgs> { } }:
pkgs.haskellPackages.callPackage
({ lib, mkDerivation, QuickCheck, quickcheck-instances, stdenv, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    testHaskellDepends = [ QuickCheck quickcheck-instances utf8-string ];
    license = lib.licenses.mit;
  }) { }
