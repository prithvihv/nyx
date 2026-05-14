{ config, pkgs, lib, ... }:

{
  services.syncthing = {
    enable = true;

    # Run as the existing `server` user so synced files live in /home/server
    # and are usable from a normal shell without chowning.
    user      = "server";
    group     = "users";
    dataDir   = "/home/server";
    configDir = "/home/server/.config/syncthing";

    # GUI stays on loopback; Caddy fronts it at https://syncthing.<domain>
    guiAddress = "127.0.0.1:8384";

    # Opens 22000/tcp+udp (sync) and 21027/udp (local discovery)
    openDefaultPorts = true;

    # Keep this false so devices/folders added via the Web UI are not wiped
    # on every nixos-rebuild. Flip to true once you want to manage them
    # declaratively below.
    overrideDevices = false;
    overrideFolders = false;

    settings = {
      gui = {
        # Required when proxied through Caddy; otherwise Syncthing rejects
        # requests whose Host header is not 127.0.0.1.
        insecureSkipHostcheck = true;
      };

      options = {
        urAccepted     = -1;
        relaysEnabled  = true;
        natEnabled     = true;
      };

      # Add devices and folders here when you want them managed by Nix, e.g.:
      #
      # devices = {
      #   "m4-mbp14" = { id = "XXXX-XXXX-XXXX-XXXX-XXXX-XXXX-XXXX-XXXX"; };
      # };
      # folders = {
      #   "Documents" = {
      #     path    = "/home/server/Sync/Documents";
      #     devices = [ "m4-mbp14" ];
      #   };
      # };
    };
  };
}
