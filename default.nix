{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, hindent, hlint, lib
  , memory, mkDerivation, nixfmt, QuickCheck, quickcheck-instances, safeio, SHA
  , stdenv, temporary, test-framework, test-framework-quickcheck2, utf8-string,
  }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    libraryHaskellDepends =
      [ base64-bytestring memory safeio SHA temporary utf8-string ];
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
