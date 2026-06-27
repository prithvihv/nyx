{ lib, ... }:

# Jellyfin managed declaratively by nixflix (was previously the stock nixpkgs
# `services.jellyfin` module). It is still published through the existing Caddy
# ingress (./ingress.nix proxies the `jellyfin` subdomain to 127.0.0.1:8096), so
# the firewall stays closed and nixflix's own reverse proxy is left disabled.
#
# Secrets are kept out of the Nix store — create these on the host before the
# first rebuild (root-owned, chmod 600), same pattern as the radarr/prowlarr keys:
#
#   /var/lib/nixflix/jellyfin-apikey      e.g. `openssl rand -hex 16 > ...`
#   /var/lib/nixflix/jellyfin-admin-pass  password for the `admin` Jellyfin user
#
# NOTE: nixflix uses /var/lib/jellyfin as its data dir — the SAME path the stock
# module used. Jellyfin's declarative setup wizard only runs when the instance is
# unconfigured, so to get a clean nixflix-managed instance (and apply the admin
# user + transcoding policy), wipe the old state once before switching:
#   systemctl stop jellyfin && rm -rf /var/lib/jellyfin /var/cache/jellyfin
{
  # # Pin `media` to its existing gid (981) so nixflix's `mkForce`d definition can't
  # # renumber it and orphan group ownership across /data (priority 30 beats mkForce).
  # users.groups.media = lib.mkOverride 30 { gid = 981; };

  nixflix.jellyfin = {
    enable = true;
    apiKey._secret = "/var/lib/nixflix/jellyfin-apikey";

    # Published via the external Caddy ingress (./ingress.nix), not nixflix's own
    # reverse proxy. Don't let nixflix emit a vhost for it.
    reverseProxy.expose = false;

    # No transcoding: direct-play only. Don't pull in GPU drivers/groups or
    # advertise any hardware acceleration.
    encoding.enableHardwareEncoding = false;
    encoding.hardwareAccelerationType = "none";

    network = {
      # Bind to loopback only; Caddy proxies from 127.0.0.1 and the firewall stays
      # closed. knownProxies lets Jellyfin trust Caddy's X-Forwarded-For.
      localNetworkAddresses = [ "127.0.0.1" ];
      knownProxies = [ "127.0.0.1" ];
    };

    # At least one admin user is required (enforced by a module assertion). The
    # setup wizard creates this user on first run using the password secret above.
    # Server-side transcoding is disabled per-user (the declarative equivalent of
    # unchecking "Allow video/audio transcoding" in the UI) — direct-play only.
    users.admin = {
      password._secret = "/var/lib/nixflix/jellyfin-admin-pass";
      policy = {
        isAdministrator = true;
        enableVideoPlaybackTranscoding = false;
        enableAudioPlaybackTranscoding = false;
      };
    };
  };
}
