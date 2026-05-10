{ lib, ... }:

{
  options.local = {
    serverIP = lib.mkOption {
      type        = lib.types.str;
      default     = "192.168.0.51";
      description = "LAN IP address of this server.";
    };

    domain = lib.mkOption {
      type        = lib.types.str;
      default     = "local.prithvihv.xyz";
      description = "Base domain for local services.";
    };
  };
}
