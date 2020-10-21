{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, cryptonite, errors
  , hindent, hlint, lib, mkDerivation, nixfmt, QuickCheck, quickcheck-instances
  , stdenv, temporary, test-framework, test-framework-quickcheck2, utf8-string,
  }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    libraryHaskellDepends =
      [ base64-bytestring cryptonite errors temporary utf8-string ];
    testHaskellDepends = [
      QuickCheck
      quickcheck-instances
      test-framework
      test-framework-quickcheck2
    ] ++ lib.optionals lint [ hindent hlint nixfmt ];
    postCheck = lib.optionalString lint ''
      hlint *.hs
      hindent --validate *.hs
      nixfmt --check *.nix
    '';
    license = lib.licenses.mit;
  }) { }
