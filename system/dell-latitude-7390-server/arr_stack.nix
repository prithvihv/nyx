{ config, lib, ... }:

# Movie *arr stack (Radarr + Prowlarr + qBittorrent + Recyclarr) provided by the
# nixflix flake input. The module is wired in via flake.nix
# (nixflix.nixosModules.default), which also pulls in VPN-Confinement.
#
# Jellyfin is intentionally NOT managed by nixflix here — it runs from the stock
# nixpkgs `services.jellyfin` module in ./jellyfin.nix (simpler/robust, and
# first-class Intel HW transcoding). nixflix still provides the shared `media`
# group that ./jellyfin.nix joins for library access.
#
# Secrets are kept out of Nix: the options below only ever reference *paths* to
# files provisioned on the host out-of-band (never values in the store). Create
# these files before the first rebuild (root-owned, chmod 600):
#
#   /var/lib/nixflix/wireguard.conf   wg-quick config for the VPN namespace
#   /var/lib/nixflix/radarr-apikey    e.g. `openssl rand -hex 16 > ...`
#   /var/lib/nixflix/prowlarr-apikey  e.g. `openssl rand -hex 16 > ...`
#
# First-run, set-in-UI items (nothing stored in this repo):
#   - Radarr / Prowlarr accounts (account-creation screen on first visit)
#   - qBittorrent password (random temp password printed to its journal)
{
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
        # Leave web-UI auth undeclared (username defaults non-null, so null it to
        # satisfy the both-or-neither assertion); create the account in the UI.
        hostConfig.username = null;
      };
    };

    prowlarr = {
      enable = true;
      config = {
        apiKey._secret = "/var/lib/nixflix/prowlarr-apikey";
        hostConfig.username = null;
      };
    };

    torrentClients.qbittorrent = {
      enable = true;
      vpn.enable = true;
      serverConfig = {
        LegalNotice.Accepted = true;
        Preferences.WebUI = {
          Username = "admin";
          # Force auth even for localhost so the Caddy upstream can't be used to
          # bypass the login, and accept the proxied (subdomain) Host header.
          LocalHostAuth = false;
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
