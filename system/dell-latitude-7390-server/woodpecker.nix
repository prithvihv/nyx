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
    # Secrets: create this file with:
    #   WOODPECKER_FORGEJO_CLIENT=<oauth app client id>
    #   WOODPECKER_FORGEJO_SECRET=<oauth app secret>
    #   WOODPECKER_AGENT_SECRET=<random string, shared with agent>
    environmentFile = "/home/phv/secrets/woodpecker/server-secrets";
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
      EnvironmentFile = "/home/phv/secrets/woodpecker/agent-secrets";
    };
    environment = {
      WOODPECKER_SERVER          = "localhost:9000";
      WOODPECKER_BACKEND         = "local";
      WOODPECKER_MAX_WORKFLOWS   = "1";
      WOODPECKER_AGENT_CONFIG_FILE = "/var/lib/woodpecker-nixos-sync-agent/agent.conf";
      WOODPECKER_AGENT_LABELS      = "type=nixos-sync,host=dell-latitude-7390-server";
      PATH                     = lib.mkForce agentPath;
    };
  };

  services.woodpecker-agents.agents.everything = {
    enable = true;
    environment = {
      WOODPECKER_SERVER        = "localhost:9000";
      WOODPECKER_BACKEND       = "local";
      WOODPECKER_MAX_WORKFLOWS = "1";
    };
    environmentFile = [ "/home/phv/secrets/woodpecker/agent-secrets" ];
    # Tools made available to pipeline steps (PATH inside the agent).
    # Mirrors what the local backend typically needs to clone & run jobs.
    path = with pkgs; [
      git
      git-lfs
      woodpecker-plugin-git
      bash
      coreutils
      nix
      restic
    ];
  };

  # Open Woodpecker UI port
  networking.firewall.allowedTCPPorts = [ 8000 ];
}
