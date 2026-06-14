{ pkgs, ... }:

{
  # ─── Grafana ──────────────────────────────────────────────────────────────
  # No Grafana login of its own. It sits behind Caddy + oauth2-proxy (Google);
  # the authenticated user is auto-signed-in from the email header oauth2-proxy
  # injects via Caddy forward-auth (X-Auth-Request-Email). Loopback only —
  # reached through Caddy at https://grafana.<domain> with `protect = true`
  # in ingress.nix, so it never gets a direct LAN port.
  # NixOS 26.05 requires an explicit Grafana DB-encryption key. Keep it out of
  # git via Grafana's $__file provider; create it once on the server:
  #   openssl rand -hex 32 | sudo tee /var/lib/grafana-secrets/secret_key >/dev/null
  #   sudo chown grafana:grafana /var/lib/grafana-secrets/secret_key
  #   sudo chmod 0640 /var/lib/grafana-secrets/secret_key
  #
  # Toggl Track API token for the Infinity datasource (Profile → API Token),
  # also kept out of git:
  #   printf '%s' '<your toggl api token>' | sudo tee /var/lib/grafana-secrets/toggl_token >/dev/null
  #   sudo chown grafana:grafana /var/lib/grafana-secrets/toggl_token
  #   sudo chmod 0640 /var/lib/grafana-secrets/toggl_token
  #   sudo systemctl restart grafana
  systemd.tmpfiles.rules = [
    "d /var/lib/grafana-secrets 0750 grafana grafana -"
  ];

  services.grafana = {
    enable = true;

    # Infinity datasource lets Grafana query arbitrary REST/JSON APIs (here, the
    # Toggl Track API) without needing a separate time-series database.
    declarativePlugins = [ pkgs.grafanaPlugins.yesoreyeram-infinity-datasource ];

    provision.datasources.settings.datasources = [
      {
        name = "Toggl Track";
        uid = "toggl-track";
        type = "yesoreyeram-infinity-datasource";
        url = "https://api.track.toggl.com";
        # Toggl uses HTTP Basic auth: username = your API token, password = the
        # literal "api_token". Grafana builds base64(user:pass) for us. The
        # token is read from a file so it stays out of git.
        basicAuth = true;
        basicAuthUser = "$__file{/var/lib/grafana-secrets/toggl_token}";
        jsonData.auth_method = "basicAuth";
        secureJsonData.basicAuthPassword = "api_token";
      }
    ];

    settings = {
      server = {
        http_addr = "127.0.0.1";
        http_port = 3000;
        domain = "grafana.local.prithvihv.xyz";
        root_url = "https://grafana.local.prithvihv.xyz/";
      };

      security.secret_key = "$__file{/var/lib/grafana-secrets/secret_key}";

      # Remove Grafana's built-in login UI entirely.
      auth = {
        disable_login_form = true;
        disable_signout_menu = true;
      };
      "auth.anonymous".enabled = false;

      # Trust the email header injected by oauth2-proxy (copy_headers in the
      # Caddy forward_auth block). auto_sign_up provisions the user on first hit.
      "auth.proxy" = {
        enabled = true;
        header_name = "X-Auth-Request-Email";
        header_property = "email";
        auto_sign_up = true;
        sync_ttl = 60;
      };

      # Single user → the auto-provisioned account becomes an org admin.
      users = {
        auto_assign_org = true;
        auto_assign_org_role = "Admin";
      };
    };
  };
}
