{ pkgs, ... }:

{
  systemd.tmpfiles.rules = [
    "d /var/lib/authentik-secrets 0750 root root -"
  ];

  services.authentik = {
    enable = true;
    environmentFile = "/var/lib/authentik-secrets/authentik.env";

    settings = {
      disable_startup_analytics = true;
      disable_update_check = true;
      error_reporting.enabled = false;
      avatars = "initials";
    };
  };
}
