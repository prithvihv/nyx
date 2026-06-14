{ ... }:

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
  systemd.tmpfiles.rules = [
    "d /var/lib/grafana-secrets 0750 grafana grafana -"
  ];

  services.grafana = {
    enable = true;
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
