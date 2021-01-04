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
      default = "${stateDirectoryRoot}/${stateDirectoryName}/password";
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
            --blobdir ${stateDirectoryRoot}/${stateDirectoryName}/blobs \
            --passwordfile ${cfg.passwordFile} \
            --port ${toString cfg.port} \
            ${concatMapStrings (p: "--peer ${p} ") cfg.peers}
        '';
      };
    };
    systemd.mounts = [{
      after = [ "bejnarkli.service" ];
      bindsTo = [ "bejnarkli.service" ];
      description = "bejnarkli blobs";
      options = "bind";
      type = "none";
      unitConfig = {
        Before = "unmount.target";
        Conflicts = "umount.target";
        DefaultDependencies = "no";
      };
      wantedBy = [ "bejnarkli.service" ];
      what = "${stateDirectoryRoot}/${stateDirectoryName}/blobs";
      where = "${stateDirectoryRoot}/${stateDirectoryName}-blobs";
    }];
  };
}
