{ lib, pkgs, ... }:

let
  # S3 API on :9900 (loopback), reached from the LAN through Caddy
  # (s3.<domain>). RPC/admin stay on loopback and aren't proxied. Garage has no
  # web console; administration is via the `garage` CLI (installed by the
  # module) and `mc`.
  apiPort = 9900;
  rpcPort = 3901;
  adminPort = 3903;
  webuiPort = 3909;
in
{
  services.garage = {
    enable = true;
    package = pkgs.garage;
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

  # `mc` CLI available system-wide (its binary is `mc`).
  environment.systemPackages = [ pkgs.minio-client ];

   # set up mc client, saves state to /home/server/.mc/config.json (0600).
  systemd.services.garage-mc-setup = {
    description = "Configure mc alias for local Garage";
    after = [ "garage.service" ];
    wants = [ "garage.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.minio-client ];
    serviceConfig = {
      Type = "oneshot";
      User = "server";
      RemainAfterExit = true;
      EnvironmentFile = "/var/lib/garage-secrets/garage.env";
    };
    script = ''
      if [ -z "''${S3_ACCESS_KEY_ID:-}" ] || [ -z "''${S3_SECRET_ACCESS_KEY:-}" ]; then
        echo "S3_ACCESS_KEY_ID/S3_SECRET_ACCESS_KEY not set; skipping (run Garage bootstrap first)" >&2
        exit 0
      fi
      for _ in $(seq 1 30); do
        if mc alias set local http://127.0.0.1:${toString apiPort} \
             "$S3_ACCESS_KEY_ID" "$S3_SECRET_ACCESS_KEY"; then
          exit 0
        fi
        sleep 2
      done
      echo "timed out waiting for Garage at 127.0.0.1:${toString apiPort}" >&2
      exit 1
    '';
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
