{ config, lib, pkgs, ... }:
let
  inherit (lib)
    concatMap concatMapStrings escapeShellArg mkEnableOption mkIf mkOption
    types;
  cfg = config.services.bejnarkli;
  stateDirectoryRoot = "/var/lib";
  stateDirectoryName = "bejnarkli";
in {
  options.services.bejnarkli = {
    enable = mkEnableOption "Run a bejnarkli blob-store-and-forward server";
    passwordFile = mkOption {
      description = "File containing the transfer password.";
      type = types.path;
      default = 8934;
    };
    peers = mkOption {
      description = "Other bejnarkli servers to forward blobs to";
      type = types.listOf types.str;
      default = [ ];
    };
    port = mkOption {
      description = "The TCP port on which to listen";
      type = types.port;
      default = 8934;
    };
  };
  config = mkIf cfg.enable {
    systemd.services.bejnarkli = {
      description = "bejnarkli blob store-and-forward server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        Restart = "on-failure";
        DynamicUser = "yes";
        StateDirectory = stateDirectoryName;
        ExecStart = ''
          ${pkgs.bejnarkli}/bin/bejnarkli \
            --blobdir ${stateDirectoryRoot}/${stateDirectoryName} \
            --passwordfile ${cfg.passwordFile} \
            --port ${toString cfg.port} \
            ${concatMapStrings (p: "--peer ${p} ") cfg.peers}
        '';
      };
    };
  };
}
