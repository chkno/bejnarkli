{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, errors, hlint, lib
  , mkDerivation, QuickCheck, quickcheck-instances, stdenv, temporary
  , test-framework, test-framework-quickcheck2, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    testHaskellDepends = [
      base64-bytestring
      errors
      QuickCheck
      quickcheck-instances
      temporary
      test-framework
      test-framework-quickcheck2
      utf8-string
    ] ++ lib.optionals lint [ hlint ];
    postCheck = lib.optionalString lint "hlint *.hs";
    license = lib.licenses.mit;
  }) { }
