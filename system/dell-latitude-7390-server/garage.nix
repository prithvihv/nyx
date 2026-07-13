{ lib, pkgs, ... }:

let
  apiPort = 9900;
  rpcPort = 3901;
  adminPort = 3903;
  webuiPort = 3909;
in
{
  services.garage = {
    enable = true;
    # garage-webui speaks the Garage 2.x admin API (/v2/...); the daemon must
    # match, otherwise the UI gets "Unknown API endpoint" on every call.
    package = pkgs.garage_2;
    environmentFile = "/var/lib/garage-secrets/garage.env";

    settings = {
      db_engine = "lmdb";
      # Single node: keep exactly one copy of every object.
      replication_factor = 1;

      rpc_bind_addr = "127.0.0.1:${toString rpcPort}";
      rpc_public_addr = "127.0.0.1:${toString rpcPort}";

      s3_api = {
        api_bind_addr = "127.0.0.1:${toString apiPort}";
        s3_region = "eu-west-1";
      };

      admin = {
        api_bind_addr = "127.0.0.1:${toString adminPort}";
      };
    };
  };

  # garage-webui: a web admin UI for Garage (buckets, keys, layout, object
  # browser). Talks to Garage's loopback admin (:3903) and S3 (:9900) APIs and
  # listens on :3909 (not in the firewall's allowed ports, so reached only
  # through Caddy at s3-admin.<domain>).
  systemd.services.garage-webui = {
    description = "Garage Web UI";
    after = [ "garage.service" ];
    wants = [ "garage.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      API_BASE_URL = "http://127.0.0.1:${toString adminPort}";
      S3_ENDPOINT_URL = "http://127.0.0.1:${toString apiPort}";
      S3_REGION = "eu-west-1";
      PORT = toString webuiPort;
    };
    serviceConfig = {
      # Provides API_ADMIN_KEY (= the Garage admin token). Kept separate from
      # garage.env so the RPC secret / S3 keys aren't exposed to the UI.
      EnvironmentFile = "/var/lib/garage-secrets/garage-webui.env";
      ExecStart = lib.getExe pkgs.garage-webui;
      DynamicUser = true;
      Restart = "on-failure";
      RestartSec = "5";
    };
  };
}
