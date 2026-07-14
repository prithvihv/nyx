{ config, pkgs, ... }:

let
  # `finalPackage` is the actual server build in use (incl. any extensions);
  # its bin/ has the client tools (vacuumdb) matching the running server.
  pg = config.services.postgresql.finalPackage;
in
{
  services.postgresql = {
    enable = true;

    package = pkgs.postgresql_17;

    # Local-only: apps on this host reach it over the Unix socket (peer auth),
    # so no TCP listener is exposed.
    enableTCPIP = false;

    settings = {
      # Autovacuum is on by default; kept explicit here to document that routine
      # dead-tuple cleanup + analyze happens continuously in the background.
      autovacuum = true;
    };
  };

  # Regular maintenance sweep across every database. Autovacuum handles the
  # steady-state churn, but a scheduled `vacuumdb --all --analyze` reclaims
  # accumulated bloat and refreshes planner statistics cluster-wide. It is a
  # plain (non-FULL) vacuum, so it takes no exclusive locks and causes no
  # downtime.
  systemd.services.postgresql-vacuum = {
    description = "VACUUM ANALYZE all PostgreSQL databases";
    after = [ "postgresql.service" ];
    requires = [ "postgresql.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      ExecStart = "${pg}/bin/vacuumdb --all --analyze --quiet";
    };
  };

  systemd.timers.postgresql-vacuum = {
    description = "Schedule regular PostgreSQL VACUUM ANALYZE";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "monthly";
      Persistent = true;
      RandomizedDelaySec = "30m";
    };
  };
}
