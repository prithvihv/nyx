{ config, lib, ... }:

# Movie *arr stack (Radarr + Prowlarr + qBittorrent + Recyclarr) provided by the
# nixflix flake input. The module is wired in via flake.nix
# (nixflix.nixosModules.default), which also pulls in VPN-Confinement.
#
# Jellyfin is also managed by nixflix, configured separately in ./jellyfin.nix
# (declarative users, plugins, and a per-user transcoding-off policy). It shares
# the same `media` group as the *arr stack for library access.
#
# Secrets are kept out of Nix: the options below only ever reference *paths* to
# files provisioned on the host out-of-band (never values in the store). Create
# these files before the first rebuild (root-owned, chmod 600):
#
#   /var/lib/nixflix/wireguard.conf      wg-quick config for the VPN namespace
#   /var/lib/nixflix/radarr-apikey       e.g. `openssl rand -hex 16 > ...`
#   /var/lib/nixflix/prowlarr-apikey     e.g. `openssl rand -hex 16 > ...`
#   /var/lib/nixflix/radarr-webui-pass   throwaway; unused under "external" auth
#   /var/lib/nixflix/prowlarr-webui-pass throwaway; unused under "external" auth
#
# First-run, set-in-UI items (nothing stored in this repo):
#   - qBittorrent password (random temp password printed to its journal)
{
  # qBittorrent otherwise writes downloads with systemd's default UMask=0022 (files
  # 0644 / dirs 0755), so group `media` gets no write bit. Radarr (also in `media`)
  # imports by hardlinking from /data/downloads to /data/media (same filesystem),
  # but fs.protected_hardlinks=1 refuses to let it hardlink a file it neither owns
  # nor can write — so every import failed with UnauthorizedAccessException ("Access
  # to the path ... is denied"). UMask=0002 makes downloads group-writable (0664 /
  # 0775), letting Radarr hardlink them.
  systemd.services.qbittorrent.serviceConfig.UMask = "0002";

  nixflix = {
    enable = true;

    mediaDir = "/data/media";
    downloadsDir = "/data/downloads";

    postgres.enable = true;

    # WireGuard namespace confining qBittorrent. accessibleFrom must include the
    # LAN so the port-mapped WebUI is reachable, plus localhost for Caddy.
    vpn = {
      enable = true;
      wgConfFile = "/var/lib/nixflix/wireguard.conf";
      accessibleFrom = [
        "192.168.0.0/24"
        "127.0.0.1/32"
      ];
    };

    # apiKey is mandatory; reference an out-of-band file so it never lands in the
    # Nix store. Login accounts are still created in each UI on first visit.
    #
    # Both are VPN-confined (same wg namespace as qBittorrent). nixflix requires
    # all interconnected services to share the namespace, so radarr + prowlarr +
    # qbittorrent must all set vpn.enable together. Note: TRaSH guides advise
    # against VPN for the *arr apps (indexer/Cloudflare blocks) — flip an
    # individual vpn.enable to false here if an indexer misbehaves.
    radarr = {
      enable = true;
      config = {
        apiKey._secret = "/var/lib/nixflix/radarr-apikey";
        hostConfig = {
          # Bind to localhost only (defaults to 0.0.0.0 without nixflix's own
          # reverse proxy, which would expose it on the LAN past Caddy/OAuth).
          # disabledForLocalAddresses makes the app skip its own login for the
          # localhost Caddy upstream; Google OAuth at Caddy is the real gate.
          # Credentials are only a fallback for non-local requests (none once
          # localhost-bound). AuthenticationRequired's default is "Enabled", so
          # it must be set non-default here to actually take effect.
          bindAddress = "127.0.0.1";
          username = "admin";
          password._secret = "/var/lib/nixflix/radarr-webui-pass";
          authenticationMethod = "external";
          authenticationRequired = "disabledForLocalAddresses";
        };
      };
    };

    prowlarr = {
      enable = true;
      config = {
        apiKey._secret = "/var/lib/nixflix/prowlarr-apikey";
        hostConfig = {
          bindAddress = "127.0.0.1";
          username = "admin";
          password._secret = "/var/lib/nixflix/prowlarr-webui-pass";
          authenticationMethod = "external";
          authenticationRequired = "disabledForLocalAddresses";
        };
      };
    };

    flaresolverr.enable = true;

    torrentClients.qbittorrent = {
      enable = true;
      vpn.enable = true;
      serverConfig = {
        LegalNotice.Accepted = true;
        # Disable seeding: stop each torrent as soon as it finishes downloading
        BitTorrent.Session = {
          GlobalMaxRatio = 0;
          MaxRatioAction = 0;
        };
        Preferences.WebUI = {
          Username = "admin";
          AuthSubnetWhitelistEnabled = true;
          AuthSubnetWhitelist = "${config.vpnNamespaces.wg.bridgeAddress}/32";
          HostHeaderValidation = false;
          CSRFProtection = false;
        };
      };
    };

    # Recyclarr syncs TRaSH-guide quality profiles + custom formats into Radarr
    # on a daily schedule. The Radarr instance config (base_url, api_key, the
    # "[SQP] SQP-1 (1080p)" profile and its custom formats) is auto-generated
    # from nixflix.radarr above — no manual config needed. Switch to "4K" for
    # the 2160p profile instead. Set cleanupUnmanagedProfiles.enable = true to
    # also delete quality profiles you didn't declare.
    recyclarr = {
      enable = true;
      # "4K" -> creates the "[SQP] SQP-1 (2160p)" profile as the managed default.
      radarrQuality = "4K";
    };
  };
}
