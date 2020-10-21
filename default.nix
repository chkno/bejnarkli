{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, errors, hindent, hlint
  , lib, mkDerivation, QuickCheck, quickcheck-instances, stdenv, temporary
  , test-framework, test-framework-quickcheck2, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    libraryHaskellDepends = [ base64-bytestring errors temporary utf8-string ];
    testHaskellDepends = [
      QuickCheck
      quickcheck-instances
      test-framework
      test-framework-quickcheck2
    ] ++ lib.optionals lint [ hindent hlint ];
    postCheck = lib.optionalString lint ''
      hlint *.hs
      hindent --validate *.hs
    '';
    license = lib.licenses.mit;
  }) { }
