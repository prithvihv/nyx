{ config, pkgs, lib, ... }:

{
  services.woodpecker-server = {
    enable = true;
    settings = {
      WOODPECKER_HOST        = "http://192.168.0.51:8000";
      WOODPECKER_SERVER_ADDR = ":8000";
      WOODPECKER_OPEN        = "false";  # no public registration

      # Forgejo OAuth - set client ID here, secret goes in secrets file
      WOODPECKER_FORGEJO     = "true";
      WOODPECKER_FORGEJO_URL = "http://192.168.0.51:9753";
    };
  };

  # Secrets: create /var/lib/woodpecker/server-secrets with:
  #   WOODPECKER_FORGEJO_CLIENT=<oauth app client id>
  #   WOODPECKER_FORGEJO_SECRET=<oauth app secret>
  #   WOODPECKER_AGENT_SECRET=<random string shared with agent>
  systemd.services.woodpecker-server.serviceConfig.EnvironmentFile =
    "/var/lib/woodpecker/server-secrets";

  services.woodpecker-agents.agents.main = {
    enable = true;
    environment = {
      WOODPECKER_SERVER        = "localhost:9000";  # gRPC
      WOODPECKER_BACKEND       = "local";
      WOODPECKER_MAX_WORKFLOWS = "1";
    };
    # Secrets: create /var/lib/woodpecker/agent-secrets with:
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
