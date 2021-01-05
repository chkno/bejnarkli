{ pkgs ? import <nixpkgs> { }, lint ? false, }:
pkgs.haskellPackages.callPackage ({ base64-bytestring, conduit, conduit-extra
  , cryptonite, cryptonite-conduit, floskell, hlint, lib, memory, mkDerivation
  , network, network-simple, network-uri, nixfmt, openssl, optparse-applicative
  , parallel-io, QuickCheck, quickcheck-instances, random, resourcet, socat
  , sqlite-simple, stdenv, streaming-commons, temporary, utf8-string, }:
  mkDerivation {
    pname = "bejnarkli";
    version = "0.2.0.1";
    src = lib.cleanSource ./.;
    libraryHaskellDepends = [
      base64-bytestring
      conduit
      conduit-extra
      cryptonite
      cryptonite-conduit
      memory
      network
      network-simple
      network-uri
      optparse-applicative
      parallel-io
      sqlite-simple
      streaming-commons
      random
      resourcet
      temporary
      utf8-string
    ];
    testHaskellDepends = [ openssl QuickCheck quickcheck-instances socat ]
      ++ lib.optionals lint [ floskell hlint nixfmt ];
    postUnpack = lib.optionalString lint ''
      sed -i '/default-language:/a \
        ghc-options:\
          -Wcompat\
          -Werror\
          -Weverything\
          -Wno-all-missed-specialisations\
          -Wno-implicit-prelude\
          -Wno-missed-specialisations\
          -Wno-safe\
          -Wno-unsafe' */bejnarkli.cabal
    '';
    postCheck = lib.optionalString lint ''
      hlint *.hs

      find app src test -name '*.hs' -exec bash -c '
        status=0
        for f;do
          diff -u --label "$f" "$f" --label "$f formatted" <(floskell < "$f")
          status=$((status || $?))
        done
        exit "$status"' - {} +

      nixfmt --check *.nix
    '';
    postInstall = ''
      patchShebangs test.sh
      ./test.sh $out/bin/bejnarkli $out/bin/bejnarkli-send
    '';
    license = lib.licenses.mit;
  }) { }
