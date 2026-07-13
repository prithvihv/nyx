{
  config,
  lib,
  ...
}:

let
  domain = config.local.domain;
  tailscaleDomain = config.local.tailscaleDomain;

  # Service catalog — single source of truth. Consumed by Caddy (vhosts, see
  # ./ingress/caddy.nix), Pi-hole (LAN DNS), and the homepage dashboard below.
  apps = [
    { name = "Forgejo"; subdomain = "git"; port = 9753; icon = "gitea.png"; description = "Git hosting"; group = "Development"; }
    { name = "Woodpecker CI"; subdomain = "ci"; port = 8000; icon = "woodpecker-ci.png"; description = "CI/CD pipelines"; group = "Development"; }
    { name = "Home Assistant"; subdomain = "home-assistant"; port = 8123; icon = "home-assistant.png"; description = "Home automation"; group = "Home"; }
    { name = "Syncthing"; subdomain = "syncthing"; port = 8384; icon = "syncthing.png"; description = "File sync"; group = "System"; protect = true; }
    { name = "Pi-hole"; subdomain = "pihole"; port = 4938; icon = "pi-hole.png"; description = "DNS & ad blocking"; group = "Network"; protect = true; }
    { name = "Apps"; subdomain = "apps"; port = 8082; icon = "homepage.png"; description = "App dashboard"; group = "System"; }
    { name = "Grafana"; subdomain = "grafana"; port = 3000; icon = "grafana.png"; description = "Dashboards & metrics"; group = "System"; protect = true; }
    { name = "Internal Apps"; subdomain = "internal-apps"; port = 4321; icon = "mdi-application-braces"; description = "Elixir/Phoenix internal apps"; group = "Development"; }
    { name = "Garage Admin"; subdomain = "s3-admin"; port = 3909; icon = "garage.png"; description = "Object storage admin UI"; group = "System"; protect = true; }
    { name = "Garage API"; subdomain = "s3"; port = 9900; icon = "garage.png"; description = "S3 object storage endpoint"; group = "System"; }
    { name = "Login"; subdomain = "auth"; port = 4180; icon = "mdi-shield-lock"; description = "Google sign-in"; group = "System"; }

    # nixflix
    { name = "Jellyfin"; subdomain = "jellyfin"; port = 8096; icon = "jellyfin.png"; description = "Media server"; group = "Media"; }
    { name = "Radarr"; subdomain = "radarr"; port = 7878; icon = "radarr.png"; description = "Movies"; group = "Media"; protect = true; }
    { name = "Prowlarr"; subdomain = "prowlarr"; port = 9696; icon = "prowlarr.png"; description = "Indexer manager"; group = "Media"; protect = true; }
    { name = "qBittorrent"; subdomain = "qbittorrent"; port = 8282; icon = "qbittorrent.png"; description = "Torrents (VPN)"; group = "Media"; protect = true; host = config.vpnNamespaces.wg.namespaceAddress; }
  ];
in
{
  imports = [ (import ./ingress/caddy.nix { inherit apps; }) ];

  services.pihole-ftl.settings.dns.hosts = map (
    app: "${config.local.serverIP} ${app.subdomain}.${domain}"
  ) apps;
  services.pihole-ftl.settings.dns.upstreams = [
    "1.1.1.1"
    "9.9.9.9"
  ];

  services.homepage-dashboard =
    let
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
              siteMonitor = "http://${app.host or "127.0.0.1"}:${toString app.port}";
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
      allowedHosts = "apps.${domain},apps.${tailscaleDomain}";

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
