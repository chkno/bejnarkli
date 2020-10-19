{ pkgs ? import <nixpkgs> { } }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, errors, lib, mkDerivation
  , QuickCheck, quickcheck-instances, stdenv, test-framework
  , test-framework-quickcheck2, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    testHaskellDepends = [
      base64-bytestring
      errors
      QuickCheck
      quickcheck-instances
      test-framework
      test-framework-quickcheck2
      utf8-string
    ];
    license = lib.licenses.mit;
  }) { }
