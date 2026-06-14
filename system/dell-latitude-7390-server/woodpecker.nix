{
  config,
  pkgs,
  lib,
  ...
}:

let
  agentPath =
    "/run/wrappers/bin:"
    + lib.makeBinPath (
      with pkgs;
      [
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
      ]
    );
in
{
  services.woodpecker-server = {
    enable = true;
    environment = {
      # Public, browser/OAuth/webhook-facing URL (must stay the domain, not an
      # IP). Sourced from the shared config.local.domain option.
      WOODPECKER_HOST = "https://ci.${config.local.domain}";
      WOODPECKER_SERVER_ADDR = ":8000";
      WOODPECKER_OPEN = "false";
      WOODPECKER_ADMIN = "prithvihv";
      WOODPECKER_FORGEJO = "true";
      # Talk to Forgejo directly on loopback, bypassing Caddy + oauth2-proxy, so
      # server-side API/config calls aren't caught by the Google gate.
      # NOTE: this same URL is used for the browser OAuth login redirect, so
      # logging into the Woodpecker UI via Forgejo will not work with a loopback
      # URL (the browser can't reach 127.0.0.1:9753).
      WOODPECKER_FORGEJO_URL = "http://127.0.0.1:9753";
    };
    environmentFile = "/var/lib/woodpecker/server-secrets";
  };

  # Define the agent as a plain service bypassing the NixOS module
  # which hardcodes DynamicUser/NoNewPrivileges. Since we define it
  # from scratch, neither is set and sudo works normally.
  systemd.services.woodpecker-nixos-sync-agent = {
    description = "Woodpecker CI Agent";
    wantedBy = [ "multi-user.target" ];
    after = [
      "woodpecker-server.service"
      "network.target"
    ];
    serviceConfig = {
      Type = "simple";
      User = "server";
      StateDirectory = "woodpecker-nixos-sync-agent";
      ExecStart = "${pkgs.woodpecker-agent}/bin/woodpecker-agent";
      Restart = "always";
      RestartSec = "15";
      EnvironmentFile = "/var/lib/woodpecker/agent-secrets";
    };
    environment = {
      WOODPECKER_SERVER = "localhost:9000";
      WOODPECKER_BACKEND = "local";
      WOODPECKER_MAX_WORKFLOWS = "1";
      WOODPECKER_AGENT_CONFIG_FILE = "/var/lib/woodpecker-nixos-sync-agent/agent.conf";
      WOODPECKER_AGENT_LABELS = "type=nixos-sync,host=dell-latitude-7390-server";
      WOODPECKER_HOSTNAME = "dell-latitude-7390-nixos-sync";
      PATH = lib.mkForce agentPath;
    };
  };

  # Woodpecker UI is reached through Caddy (ci.<domain>); no direct LAN port.
  # The agent talks to the server over gRPC on loopback (:9000).
}
