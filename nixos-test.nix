import (<nixpkgs/nixos/tests/make-test-python.nix>) ({ pkgs, lib, ... }:
  let
    passwordFile = pkgs.writeText "bejnarkli-test-secret" "secret";
    testBlob = pkgs.writeText "bejnarkli-test-blob" "contents";
    bejnarkliOverlay = final: prev: {
      bejnarkli = import ./default.nix { inherit pkgs; };
    };
    serverConfig = { config, pkgs, ... }: {
      imports = [ ./nixos-module.nix ];
      nixpkgs.overlays = [ bejnarkliOverlay ];
      services.bejnarkli = {
        inherit passwordFile;
        enable = true;
        peers = [ "b1" "b2" "b3" ];
      };
      networking.firewall.allowedTCPPorts = [ config.services.bejnarkli.port ];
    };
  in {
    name = "bejnarkli-nixos-module-test";
    nodes = {
      b1 = serverConfig;
      b2 = serverConfig;
      b3 = serverConfig;
      client = { pkgs, ... }: {
        nixpkgs.overlays = [ bejnarkliOverlay ];
        environment.systemPackages = [ pkgs.bejnarkli ];
      };
    };
    testScript = ''
      start_all()
      client.succeed(
          "bejnarkli-send --durability 1 --passwordfile ${passwordFile} ${testBlob} b1 b2"
      )
      for b in [b1, b2, b3]:
          b.wait_until_succeeds('[[ "$(cat /var/lib/bejnarkli/*)" == contents ]]')
    '';
  })
