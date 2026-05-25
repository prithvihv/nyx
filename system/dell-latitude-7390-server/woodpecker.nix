{ config, pkgs, lib, ... }:

let
  agentPath = "/run/wrappers/bin:" + lib.makeBinPath (with pkgs; [
    woodpecker-plugin-git
    git
    git-lfs
    bash
    coreutils
    openssh
    nix
    systemd
    config.system.build.nixos-rebuild
    restic
  ]);
in
{
  services.woodpecker-server = {
    enable = true;
    environment = {
      WOODPECKER_HOST        = "https://ci.local.prithvihv.xyz";
      WOODPECKER_SERVER_ADDR = ":8000";
      WOODPECKER_OPEN        = "false";
      WOODPECKER_ADMIN       = "prithvihv";
      WOODPECKER_FORGEJO     = "true";
      WOODPECKER_FORGEJO_URL = "https://git.local.prithvihv.xyz";
    };
    environmentFile = "/var/lib/woodpecker/server-secrets";
  };

  # Define the agent as a plain service bypassing the NixOS module
  # which hardcodes DynamicUser/NoNewPrivileges. Since we define it
  # from scratch, neither is set and sudo works normally.
  systemd.services.woodpecker-nixos-sync-agent = {
    description = "Woodpecker CI Agent";
    wantedBy    = [ "multi-user.target" ];
    after       = [ "woodpecker-server.service" "network.target" ];
    serviceConfig = {
      Type            = "simple";
      User            = "server";
      StateDirectory  = "woodpecker-nixos-sync-agent";
      ExecStart       = "${pkgs.woodpecker-agent}/bin/woodpecker-agent";
      Restart         = "always";
      RestartSec      = "15";
      EnvironmentFile = "/var/lib/woodpecker/agent-secrets";
    };
    environment = {
      WOODPECKER_SERVER          = "localhost:9000";
      WOODPECKER_BACKEND         = "local";
      WOODPECKER_MAX_WORKFLOWS   = "1";
      WOODPECKER_AGENT_CONFIG_FILE = "/var/lib/woodpecker-nixos-sync-agent/agent.conf";
      WOODPECKER_AGENT_LABELS      = "type=nixos-sync,host=dell-latitude-7390-server";
      WOODPECKER_HOSTNAME          = "dell-latitude-7390-nixos-sync";
      PATH                     = lib.mkForce agentPath;
    };
  };

  # Open Woodpecker UI port
  networking.firewall.allowedTCPPorts = [ 8000 ];
}
