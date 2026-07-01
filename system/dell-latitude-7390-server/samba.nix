{ config, ... }:

# Read-only SMB share of the media library so it can be streamed/browsed
# directly in VLC (smb://192.168.0.51), mounted in macOS Finder (⌘K →
# smb://…) or Windows Explorer, in addition to Jellyfin (./nixflix.nix).
#
# Guest access, no passwords: SMB credentials would have to live in a stateful
# smbpasswd db (not declarative), and this LAN is already gated by the router
# with no port-forwarding. The share is `read only`, so it can only stream —
# never modify or delete — the library.
#
# Permissions: the media files are owned by the `media` group (that's how
# Jellyfin reads them). `force group = media` makes every guest connection use
# that group's read bits, so no world-readable perms are required.
let
  mediaDir = "/data/media";
in
{
  services.samba = {
    enable = true;
    openFirewall = true;

    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "nixflix media";
        "security" = "user";

        # Allow anonymous access: unknown users are mapped to the guest account
        # instead of being rejected.
        "map to guest" = "bad user";
        "guest account" = "nobody";

        # Modern clients only; keeps things fast and avoids the ancient SMB1.
        "server min protocol" = "SMB2";

        # Headless server — no printer sharing.
        "load printers" = "no";
        "printing" = "bsd";
        "printcap name" = "/dev/null";
        "disable spoolss" = "yes";
      };

      media = {
        "path" = mediaDir;
        "browseable" = "yes";
        "read only" = "yes";
        "guest ok" = "yes";

        # Connect as nobody but with the `media` group's read permissions.
        "force user" = "nobody";
        "force group" = "media";
      };
    };
  };
}
