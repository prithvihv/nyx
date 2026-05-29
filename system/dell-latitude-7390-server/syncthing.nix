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

    overrideDevices = true;
    overrideFolders = true;

    settings = {
      gui = {
        # Required when proxied through Caddy; otherwise Syncthing rejects
        # requests whose Host header is not 127.0.0.1.
        insecureSkipHostcheck = true;
      };

      options = {
        urAccepted            = -1;   # no usage reporting
        relaysEnabled         = false;
        globalAnnounceEnabled = false;
        localAnnounceEnabled  = false;
        natEnabled            = false;
      };

      # ── Devices ─────────────────────────────────────────────────────────
      # Get a device ID from: Actions → Show ID in the Syncthing GUI, or
      # `syncthing -home=/home/server/.config/syncthing show-device-id` on the host.
      devices = {
        "mb-wooga" = { id = "7WPE4CB-3EJACSZ-HXHDKJG-NHPTHSJ-JGOFBOO-2QIUKXU-XBKWZY6-N6RBQAL"; addresses = [ "tcp://192.168.0.54:22000" ]; };
        "m4-mbp14" = { id = "PHGI2GS-T5IWYZB-4VCPVUQ-AD5KTQM-7HCEJVD-6ESYUVW-WZI74LD-FDZXNAK"; addresses = [ "tcp://192.168.0.183:22000" ]; };
      };

      # ── Folders ─────────────────────────────────────────────────────────
      # `id` must match the folder ID set on every peer sharing it.
      folders = {
        "phv-logs" = {
          id      = "phv-logs";
          path    = "/home/server/phv-logs";
          devices = [
            "mb-wooga"
            "m4-mbp14"
          ];
        };
        "resume-cuts" = {
          id      = "phv-logs";
          path    = "/home/server/resume-cuts";
          devices = [
            "mb-wooga"
            "m4-mbp14"
          ];
        };
      };
    };
  };
}
