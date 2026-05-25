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
    # Secrets: create this file with:
    #   WOODPECKER_FORGEJO_CLIENT=<oauth app client id>
    #   WOODPECKER_FORGEJO_SECRET=<oauth app secret>
    #   WOODPECKER_AGENT_SECRET=<random string, shared with agent>
    # NOTE: this path is a NixOS-managed symlink (see
    # systemd.tmpfiles.rules at the bottom of this file) that points at
    # the real secret on the LUKS-encrypted /home/phv volume. Keeping
    # the unit's EnvironmentFile path under /var/lib means systemd's
    # auto-generated RequiresMountsFor= dep resolves to the root mount
    # instead of home-phv.mount. The root mount is never restarted by
    # switch-to-configuration, so this unit isn't either.
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
      # See note on services.woodpecker-server.environmentFile above:
      # this is a /var/lib symlink to the real secret on /home/phv to
      # avoid systemd auto-deriving RequiresMountsFor=/home/phv.
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

  # ── Declarative symlinks to LUKS-backed secrets ──────────────────────
  # Actual encrypted secrets live on the LUKS /home/phv mount. The unit
  # files reference these stable /var/lib paths so systemd's auto-
  # generated RequiresMountsFor= dep resolves to the root mount, not to
  # home-phv.mount. Without this, switch-to-configuration cascade-
  # restarts these units during every `nixos-rebuild switch`, which
  # SIGTERMs the agent mid-build.
  #
  # `L+` = create symlink, overwriting any existing file/symlink at the
  # target path. systemd-tmpfiles applies this on every boot (and on
  # `systemd-tmpfiles --create`).
  systemd.tmpfiles.rules = [
    "d /var/lib/woodpecker 0750 root root - -"
    "L+ /var/lib/woodpecker/server-secrets - - - - /home/phv/secrets/woodpecker/server-secrets"
    "L+ /var/lib/woodpecker/agent-secrets  - - - - /home/phv/secrets/woodpecker/agent-secrets"
  ];
}
