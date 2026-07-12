{ config, lib, ... }:

let
  cfg = config.local;
in
{
  options.local = {
    serverIP = lib.mkOption {
      type        = lib.types.str;
      default     = "192.168.0.51";
      description = "LAN IP address of this server.";
    };

    baseDomain = lib.mkOption {
      type        = lib.types.str;
      default     = "prithvihv.xyz";
      description = "Apex domain; each access tree is a subdomain of this.";
    };

    domain = lib.mkOption {
      type        = lib.types.str;
      default     = "local.${cfg.baseDomain}";
      description = "Base domain for local (LAN) services.";
    };

    tailscaleDomain = lib.mkOption {
      type        = lib.types.str;
      default     = "tailscale.${cfg.baseDomain}";
      description = "Base domain for services reached over Tailscale.";
    };
  };
}
