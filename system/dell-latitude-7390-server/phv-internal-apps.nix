{ config, ... }:

# The phv-apps Elixir/Phoenix umbrella release, wired in via the
# `phv-internal-apps` flake input (flake.nix -> phv-internal-apps.nixosModules.default,
# which injects the per-system `phv` release package). Published through the
# Caddy ingress as internal-apps.<domain>; the app binds to loopback:4321 and is
# only reachable via Caddy (80/443).
#
# Database: local PostgreSQL over the Unix socket with peer auth — the service
# runs as the `phv-internal-apps` system user and connects as the Postgres role
# of the same name, so no DB password lives anywhere. The role + database are
# provisioned below (ensureUsers/ensureDatabases); the release runs its own Ecto
# migrations on boot.
#
# Secrets never live in the Nix store. Provision before the first rebuild
# (root-owned, chmod 600):
#
#   /var/lib/phv-internal-apps/secrets   KEY=VALUE lines, must set:
#                                          SECRET_KEY_BASE=...   (mix phx.gen.secret)
let
  # Peer auth requires the OS user, the Postgres role, and (via
  # ensureDBOwnership) the database to all share one name.
  user = "phv-internal-apps";
  dbName = user;
in
{
  users.users.${user} = {
    isSystemUser = true;
    group = user;
    description = "phv-internal-apps service user";
  };
  users.groups.${user} = { };

  services.postgresql = {
    ensureDatabases = [ dbName ];
    ensureUsers = [
      {
        name = user;
        ensureDBOwnership = true;
      }
    ];
  };

  services.phv-internal-apps = {
    enable = true;
    host = "internal-apps.${config.local.domain}";
    port = 4321;

    # Peer auth over the local socket: the OS user must match the Postgres role
    # name, so `user` and `database.user` are both the `phv-internal-apps` role
    # provisioned above (ensureUsers).
    inherit user;
    database.user = user;
    database.socketDir = "/run/postgresql";
    database.name = dbName;

    environmentFile = "/var/lib/phv-internal-apps/secrets";
  };
}
