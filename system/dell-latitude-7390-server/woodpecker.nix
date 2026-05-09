{ config, pkgs, lib, ... }:

let
  # Declaratively define what tools the woodpecker agent has access to.
  # Add packages here as needed — Nix manages the paths.
  agentEnv = pkgs.buildEnv {
    name = "woodpecker-agent-env";
    paths = with pkgs; [
      git
    ];
  };
in
{
  services.woodpecker-server = {
    enable = true;
    environment = {
      WOODPECKER_HOST        = "http://192.168.0.51:8000";
      WOODPECKER_SERVER_ADDR = ":8000";
      WOODPECKER_OPEN        = "false";
      WOODPECKER_ADMIN       = "prithvihv";
      WOODPECKER_FORGEJO     = "true";
      WOODPECKER_FORGEJO_URL = "http://192.168.0.51:9753";
    };
    # Secrets: create this file with:
    #   WOODPECKER_FORGEJO_CLIENT=<oauth app client id>
    #   WOODPECKER_FORGEJO_SECRET=<oauth app secret>
    #   WOODPECKER_AGENT_SECRET=<random string, shared with agent>
    environmentFile = "/var/lib/woodpecker/server-secrets";
  };

  services.woodpecker-agents.agents.main = {
    enable = true;
    environment = {
      WOODPECKER_SERVER        = "localhost:9000";
      WOODPECKER_BACKEND       = "local";
      WOODPECKER_MAX_WORKFLOWS = "1";
      PATH                     = "${agentEnv}/bin:/run/wrappers/bin";
    };
    # Secrets: create this file with:
    #   WOODPECKER_AGENT_SECRET=<same random string as server>
    environmentFile = [ "/var/lib/woodpecker/agent-secrets" ];
  };

  # Allow the woodpecker agent to run nixos-rebuild only
  security.sudo.extraRules = [
    {
      users = [ "woodpecker" ];
      commands = [
        {
          command = "/run/current-system/sw/bin/nixos-rebuild";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  # Open Woodpecker UI port
  networking.firewall.allowedTCPPorts = [ 8000 ];
}
