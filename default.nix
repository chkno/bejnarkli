{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, hindent, hlint, lib
  , memory, mkDerivation, nixfmt, QuickCheck, quickcheck-instances, SHA, stdenv
  , temporary, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    libraryHaskellDepends =
      [ base64-bytestring memory SHA temporary utf8-string ];
    testHaskellDepends = [ QuickCheck quickcheck-instances ]
      ++ lib.optionals lint [ hindent hlint nixfmt ];
    postCheck = lib.optionalString lint ''
      hlint *.hs
      hindent --validate *.hs
      nixfmt --check *.nix
    '';
    license = lib.licenses.mit;
  }) { }
