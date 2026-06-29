{ config, ... }:

# Full media stack from the nixflix flake input (wired in via flake.nix /
# nixflix.nixosModules.default, which also pulls in VPN-Confinement): Radarr,
# Prowlarr, qBittorrent, FlareSolverr, Recyclarr, and Jellyfin. All share the
# `media` group for library access.
#
# Secrets never live in the Nix store — the options below only reference *paths*
# to files provisioned out-of-band (root-owned, chmod 600). Create before the
# first rebuild:
#
#   /var/lib/nixflix/wireguard.conf        wg-quick config for the VPN namespace
#   /var/lib/nixflix/radarr-apikey         openssl rand -hex 16 > ...
#   /var/lib/nixflix/prowlarr-apikey       openssl rand -hex 16 > ...
#   /var/lib/nixflix/radarr-webui-pass     throwaway; unused under "external" auth
#   /var/lib/nixflix/prowlarr-webui-pass   throwaway; unused under "external" auth
#   /var/lib/nixflix/jellyfin-apikey       openssl rand -hex 16 > ...
#   /var/lib/nixflix/jellyfin-admin-pass   password for the `admin` Jellyfin user
#   /var/lib/nixflix/opensubtitles-pass    OpenSubtitles.com account password
{
  # qBittorrent writes downloads 0644/0755 under systemd's default UMask, leaving
  # group `media` no write bit — so Radarr's hardlink import trips
  # fs.protected_hardlinks and fails. 0002 makes downloads group-writable.
  systemd.services.qbittorrent.serviceConfig.UMask = "0002";

  nixflix = {
    enable = true;

    mediaDir = "/data/media";
    downloadsDir = "/data/downloads";

    postgres.enable = true;

    # accessibleFrom must include the LAN (port-mapped WebUI) and localhost (Caddy).
    vpn = {
      enable = true;
      wgConfFile = "/var/lib/nixflix/wireguard.conf";
      accessibleFrom = [
        "192.168.0.0/24"
        "127.0.0.1/32"
      ];
    };

    # Bind to localhost so the apps are only reachable via Caddy (Google OAuth is
    # the real gate); "external" auth + disabledForLocalAddresses skips their own
    # login for the localhost upstream. Not VPN-confined (TRaSH advises against it
    # for *arr apps) — only qBittorrent runs in the wg namespace.
    radarr = {
      enable = true;
      config = {
        apiKey._secret = "/var/lib/nixflix/radarr-apikey";
        hostConfig = {
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

    # Auto-wired into Prowlarr as a "FlareSolverr" indexer proxy (tagged
    # "flaresolverr"); tag the indexers that need challenge-solving in the UI.
    flaresolverr.enable = true;

    torrentClients.qbittorrent = {
      enable = true;
      vpn.enable = true;
      serverConfig = {
        LegalNotice.Accepted = true;
        # Stop each torrent as soon as it finishes downloading (no seeding).
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

    # Daily TRaSH-guide profile/custom-format sync into Radarr; "4K" creates the
    # "[SQP] SQP-1 (2160p)" profile as the managed default.
    recyclarr = {
      enable = true;
      radarrQuality = "4K";
    };

    # Published via the external Caddy ingress (./ingress.nix), so nixflix's own
    # vhost is disabled and it binds to loopback. Hardware transcoding off
    # (direct-play only); the admin user is created by the setup wizard on first
    # run (wipe /var/lib/jellyfin once when switching instances).
    jellyfin = {
      enable = true;
      apiKey._secret = "/var/lib/nixflix/jellyfin-apikey";
      reverseProxy.expose = false;
      encoding.enableHardwareEncoding = false;
      encoding.hardwareAccelerationType = "none";
      network = {
        localNetworkAddresses = [ "127.0.0.1" ];
        knownProxies = [ "127.0.0.1" ];
      };
      plugins = {
        "Open Subtitles" = {
          enable = true;
          config = {
            Username = "server_phv_de";
            Password._secret = "/var/lib/nixflix/opensubtitles-pass";
          };
        };

        "Subtitle Extract" = {
          enable = true;
          config.ExtractionDuringLibraryScan = true;
        };
      };

      libraries.Movies = {
        subtitleDownloadLanguages = [ "eng" ];
        saveSubtitlesWithMedia = true;
        requirePerfectSubtitleMatch = false;
        skipSubtitlesIfEmbeddedSubtitlesPresent = true;
      };

      users.admin = {
        password._secret = "/var/lib/nixflix/jellyfin-admin-pass";
        policy = {
          isAdministrator = true;
          enableVideoPlaybackTranscoding = false;
          enableAudioPlaybackTranscoding = false;
        };
      };
    };
  };
}
