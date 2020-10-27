{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, hindent, hlint, lib
  , memory, mkDerivation, network-simple, nixfmt, openssl, QuickCheck
  , quickcheck-instances, SHA, socat, stdenv, temporary, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.0.1.0";
    src = lib.cleanSource ./.;
    libraryHaskellDepends =
      [ base64-bytestring memory network-simple SHA temporary utf8-string ];
    testHaskellDepends = [ openssl QuickCheck quickcheck-instances socat ]
      ++ lib.optionals lint [ hindent hlint nixfmt ];
    postCheck = lib.optionalString lint ''
      hlint *.hs
      hindent --validate *.hs
      nixfmt --check *.nix
    '';
    postInstall = ''
      patchShebangs test.sh
      ./test.sh $out/bin/bejnarkli
    '';
    license = lib.licenses.mit;
  }) { }
