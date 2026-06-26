{ ... }:

# Jellyfin via the stock nixpkgs `services.jellyfin` module (not nixflix). It is
# published through the existing Caddy ingress (./ingress.nix proxies the
# `jellyfin` subdomain to 127.0.0.1:8096), so the firewall stays closed.
#
# Transcoding is intentionally NOT set up: no hardware acceleration, no GPU
# drivers/groups. To additionally forbid software transcoding (direct-play
# only), uncheck "Allow video/audio transcoding" per user in the Jellyfin UI
# (Dashboard -> Users -> <user> -> playback permissions) — the stock module has
# no declarative option for that.
#
# Library/users are configured once in the Jellyfin web UI on first run.
{
  services.jellyfin = {
    enable = true;
    # Reached only via Caddy on localhost; do not expose 8096/8920 on the LAN.
    openFirewall = false;
  };

  # Jellyfin runs as its own user; add the nixflix-managed `media` group so it
  # can read the library.
  users.users.jellyfin.extraGroups = [ "media" ];
}
