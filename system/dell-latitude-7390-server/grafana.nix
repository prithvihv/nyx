{ pkgs, ... }:

{
  systemd.tmpfiles.rules = [
    "d /var/lib/grafana-secrets 0750 root root -"
  ];
  systemd.services.grafana.serviceConfig.EnvironmentFile = "/var/lib/grafana-secrets/grafana.env";

  services.grafana = {
    enable = true;
    declarativePlugins = [ pkgs.grafanaPlugins.yesoreyeram-infinity-datasource ];

    provision.datasources.settings.datasources = [
      {
        name = "Toggl Track";
        uid = "toggl-track";
        type = "yesoreyeram-infinity-datasource";
        url = "https://api.track.toggl.com";
        basicAuth = true;
        basicAuthUser = "$__env{TOGGL_API_TOKEN}";
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

      # Required since NixOS 26.05. Value comes from the env file above.
      security.secret_key = "$__env{GRAFANA_SECRET_KEY}";

      # Remove Grafana's built-in login UI entirely.
      auth = {
        disable_login_form = true;
        disable_signout_menu = true;
      };
      "auth.anonymous".enabled = false;

      # Trust the email header injected by oauth2-proxy via Caddy forward-auth.
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
