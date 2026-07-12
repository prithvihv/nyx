{ pkgs, ... }:

let
  # S3 API on :9900, web console on :9901. Both loopback-only; reached from the
  # LAN through Caddy (s3.<domain> / minio.<domain>). NB: MinIO's usual
  # 9000/9001 collide with Woodpecker's gRPC server (:9000), so we shift up.
  apiPort = 9900;
  consolePort = 9901;
in
{
  systemd.tmpfiles.rules = [
    "d /var/lib/minio-secrets 0750 root root -"
    "d /var/lib/minio 0750 minio minio -"
    "d /var/lib/minio/data 0750 minio minio -"
    "d /var/lib/minio/config 0750 minio minio -"
  ];

  services.minio = {
    enable = true;

    listenAddress = "127.0.0.1:${toString apiPort}";
    consoleAddress = "127.0.0.1:${toString consolePort}";

    dataDir = [ "/var/lib/minio/data" ];
    configDir = "/var/lib/minio/config";

    region = "eu-west-1";
    rootCredentialsFile = "/var/lib/minio-secrets/minio.env";
  };

  # `mc` CLI available system-wide (its binary is `mc`).
  environment.systemPackages = [ pkgs.minio-client ];

  systemd.services.minio-client-setup = {
    description = "Configure mc alias for local MinIO";
    after = [ "minio.service" ];
    wants = [ "minio.service" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.minio-client ];
    serviceConfig = {
      Type = "oneshot";
      User = "server";
      RemainAfterExit = true;
      EnvironmentFile = "/var/lib/minio-secrets/minio.env";
    };
    # Retry: `after minio.service` only guarantees the unit started, not that
    # the API is accepting connections yet.
    script = ''
      for _ in $(seq 1 30); do
        if mc alias set local http://127.0.0.1:${toString apiPort} \
             "$MINIO_ROOT_USER" "$MINIO_ROOT_PASSWORD"; then
          exit 0
        fi
        sleep 2
      done
      echo "timed out waiting for MinIO at 127.0.0.1:${toString apiPort}" >&2
      exit 1
    '';
  };
}
