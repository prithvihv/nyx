{ config, pkgs, ... }:

{
  services.forgejo = {
    enable = true;

    lfs.enable = true;

    settings = {
      server = {
        DOMAIN = "git.local.prithvihv.xyz";
        ROOT_URL = "https://git.local.prithvihv.xyz/";
        # Loopback only; reached from the LAN through Caddy (git.<domain>).
        HTTP_ADDR = "127.0.0.1";
        HTTP_PORT = 9753;

        SSH_DOMAIN = "192.168.0.51";
        SSH_PORT = 2222;

        LOCAL_ROOT_URL = "http://127.0.0.1:9753/";

        START_SSH_SERVER = true;
        SSH_LISTEN_HOST = "0.0.0.0";
        SSH_LISTEN_PORT = 2222;
      };

      actions.ENABLED = false;

      webhook.ALLOWED_HOST_LIST = "loopback,private";
    };
  };

  # Open ports in the firewall. Only Git SSH is exposed directly; all web UIs
  # are reached through Caddy (80/443) so oauth2-proxy/app auth can't be
  # bypassed by hitting a backend port directly.
  networking.firewall.allowedTCPPorts = [
    2222 # Forgejo Git over SSH
  ];
}
