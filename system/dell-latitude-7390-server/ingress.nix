{
  config,
  pkgs,
  lib,
  ...
}:

let
  domain = config.local.domain;

  # oauth2-proxy forward-auth endpoint (loopback). The public https://auth.<domain>
  # vhost proxies to it so Google sign-in / callback work; protected vhosts ask
  # it whether the request is authenticated.
  authProxy = "127.0.0.1:4180";
  authDomain = "auth.${domain}";

  # ── Add a new app here and it appears in Caddy, DNS and Homepage ───────
  # `protect = true` gates the app behind oauth2-proxy (Google login). Only
  # worthwhile for apps reachable solely via Caddy (loopback / firewalled) and
  # without their own API/webhook/mobile clients.
  apps = [
    {
      name = "Forgejo";
      subdomain = "git";
      port = 9753;
      icon = "gitea.png";
      description = "Git hosting";
      group = "Development";
      protect = false;
    }
    {
      name = "Woodpecker CI";
      subdomain = "ci";
      port = 8000;
      icon = "woodpecker-ci.png";
      description = "CI/CD pipelines";
      group = "Development";
      protect = false;
    }
    {
      name = "Home Assistant";
      subdomain = "home-assistant";
      port = 8123;
      icon = "home-assistant.png";
      description = "Home automation";
      group = "Home";
      protect = false;
    }
    {
      name = "Syncthing";
      subdomain = "syncthing";
      port = 8384;
      icon = "syncthing.png";
      description = "File sync";
      group = "System";
      protect = false;
    }
    {
      name = "Pi-hole";
      subdomain = "pihole";
      port = 4938;
      icon = "pi-hole.png";
      description = "DNS & ad blocking";
      group = "Network";
      protect = false;
    }
    {
      name = "Apps";
      subdomain = "apps";
      port = 8082;
      icon = "homepage.png";
      description = "App dashboard";
      group = "System";
      protect = false;
    }
    {
      name = "Login";
      subdomain = "auth";
      port = 4180; # oauth2-proxy
      icon = "mdi-shield-lock";
      description = "Google sign-in";
      group = "System";
      protect = false;
    }
  ];

  # Caddy config for one app: plain reverse proxy, or gated behind oauth2-proxy
  # (Google login) when `protect = true`.
  appExtraConfig =
    app:
    if app.protect then
      ''
        forward_auth ${authProxy} {
          uri /oauth2/auth
          copy_headers X-Auth-Request-User X-Auth-Request-Email

          @error status 401
          handle_response @error {
            redir * https://${authDomain}/oauth2/start?rd={scheme}://{host}{uri}
          }
        }
        reverse_proxy 127.0.0.1:${toString app.port}
      ''
    else
      ''
        reverse_proxy 127.0.0.1:${toString app.port}
      '';
in
{
  # ── Caddy reverse proxy ────────────────────────────────────────────────
  services.caddy = {
    enable = true;

    package = pkgs.caddy.withPlugins {
      plugins = [ "github.com/caddy-dns/cloudflare@v0.2.4" ];
      hash = "sha256-8yZDrejNKsaUnUaTUFYbarWNmxafqp2z2rWo+XRsxV8=";
    };

    globalConfig = ''
      acme_dns cloudflare {$CLOUDFLARE_API_TOKEN}
    '';

    virtualHosts = builtins.listToAttrs (
      map (app: {
        name = "${app.subdomain}.${domain}";
        value.extraConfig = appExtraConfig app;
      }) apps
    );
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile = [ "/var/lib/caddy/secrets" ];
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  # ── Pi-hole local DNS overrides ────────────────────────────────────────
  services.pihole-ftl.settings.dns.hosts = map (
    app: "${config.local.serverIP} ${app.subdomain}.${domain}"
  ) apps;
  services.pihole-ftl.settings.dns.upstreams = [
    "1.1.1.1"
    "9.9.9.9"
  ];

  # ── Homepage dashboard ─────────────────────────────────────────────────
  services.homepage-dashboard =
    let
      # Static items that appear in Homepage only (no Caddy/DNS entry)
      staticItems = [
        {
          group = "Network";
          name = "Router";
          icon = "router.png";
          href = "http://192.168.0.1/";
          description = "TP-Link router";
        }
      ];

      allGroups = lib.unique ((map (a: a.group) apps) ++ (map (a: a.group) staticItems));

      makeGroup = group: {
        ${group} =
          (map (app: {
            ${app.name} = {
              icon = app.icon;
              href = "https://${app.subdomain}.${domain}";
              description = app.description;
              siteMonitor = "http://127.0.0.1:${toString app.port}";
            };
          }) (builtins.filter (a: a.group == group) apps))
          ++ (map (item: {
            ${item.name} = {
              icon = item.icon;
              href = item.href;
              description = item.description;
            };
          }) (builtins.filter (a: a.group == group) staticItems));
      };
    in
    {
      enable = true;
      allowedHosts = "apps.${domain}";

      settings = {
        title = "prithvihv";
        theme = "dark";
        color = "slate";
        language = "en";
        target = "_blank";
        headerStyle = "clean";
        hideVersion = true;
        disableUpdateCheck = true;
        disableIndexing = true;
      };

      services = map makeGroup allGroups;
    };
}
