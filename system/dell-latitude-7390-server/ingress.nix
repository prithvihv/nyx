{ config, pkgs, lib, ... }:

let
  domain = config.local.domain;

  # ── Add a new app here and it appears in Caddy, DNS and Homepage ───────
  apps = [
    { name = "Forgejo";        subdomain = "git";            port = 9753; icon = "gitea.png";          description = "Git hosting";       group = "Development"; }
    { name = "Woodpecker CI";  subdomain = "ci";             port = 8000; icon = "woodpecker-ci.png";  description = "CI/CD pipelines";   group = "Development"; }
    { name = "Home Assistant"; subdomain = "home-assistant"; port = 8123; icon = "home-assistant.png"; description = "Home automation";   group = "Home";        }
    { name = "Pi-hole";        subdomain = "pihole";         port = 4938; icon = "pi-hole.png";        description = "DNS & ad blocking"; group = "Network";     }
    { name = "Apps";           subdomain = "apps";           port = 8082; icon = "homepage.png";       description = "App dashboard";     group = "System";      }
  ];
in
{
  # ── Caddy reverse proxy ────────────────────────────────────────────────
  services.caddy = {
    enable = true;

    package = pkgs.caddy.withPlugins {
      plugins = [ "github.com/caddy-dns/cloudflare@v0.2.4" ];
      hash    = "sha256-4WF7tIx8d6O/Bd0q9GhMch8lS3nlR5N3Zg4ApA3hrKw=";
    };

    globalConfig = ''
      acme_dns cloudflare {$CLOUDFLARE_API_TOKEN}
    '';

    virtualHosts = builtins.listToAttrs (
      map (app: {
        name  = "${app.subdomain}.${domain}";
        value.extraConfig = "reverse_proxy 127.0.0.1:${toString app.port}";
      }) apps
    );
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile = [ "/var/lib/caddy/secrets" ];
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # ── Pi-hole local DNS overrides ────────────────────────────────────────
  services.pihole-ftl.settings.dns.hosts =
    map (app: "${config.local.serverIP} ${app.subdomain}.${domain}") apps;
  services.pihole-ftl.settings.dns.upstreams = [ "1.1.1.1" "9.9.9.9" ];

  # ── Homepage dashboard ─────────────────────────────────────────────────
  services.homepage-dashboard =
    let
      # Static items that appear in Homepage only (no Caddy/DNS entry)
      staticItems = [
        { group = "Network"; name = "Router"; icon = "router.png"; href = "http://192.168.0.1/"; description = "TP-Link router"; }
      ];

      allGroups = lib.unique ((map (a: a.group) apps) ++ (map (a: a.group) staticItems));

      makeGroup = group: {
        ${group} =
          (map (app: {
            ${app.name} = {
              icon        = app.icon;
              href        = "https://${app.subdomain}.${domain}";
              description = app.description;
              siteMonitor = "http://127.0.0.1:${toString app.port}";
            };
          }) (builtins.filter (a: a.group == group) apps))
          ++
          (map (item: {
            ${item.name} = {
              icon        = item.icon;
              href        = item.href;
              description = item.description;
            };
          }) (builtins.filter (a: a.group == group) staticItems));
      };
    in
    {
      enable       = true;
      allowedHosts = "apps.${domain}";

      settings = {
        title              = "prithvihv";
        theme              = "dark";
        color              = "slate";
        language           = "en";
        target             = "_blank";
        headerStyle        = "clean";
        hideVersion        = true;
        disableUpdateCheck = true;
        disableIndexing    = true;
      };

      services = map makeGroup allGroups;
    };
}
