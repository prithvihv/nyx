# Caddy reverse proxy — TLS termination (Cloudflare DNS-01) and per-endpoint
# vhost generation. The `apps` catalog is passed in from ../ingress.nix (the
# single source of truth), so this file owns only the Caddy-specific wiring.
{ apps }:

{ config, pkgs, ... }:

let
  domain = config.local.domain;
  tailscaleDomain = config.local.tailscaleDomain;

  # Auth endpoints — one per hostname tree, each backed by its own oauth2-proxy
  # instance (see oauth2-proxy.nix) so cookies/sessions stay isolated and each
  # tree's OAuth callback host is reachable from where the tree is used:
  #   local     -> primary instance :4180 (auth.local.<apex>, LAN via Pi-hole)
  #   tailscale -> second  instance :4181 (auth.tailscale.<apex>, tailnet)
  localEndpoint = {
    base = domain;
    authProxy = "127.0.0.1:4180";
  };
  tailscaleEndpoint = {
    base = tailscaleDomain;
    authProxy = "127.0.0.1:4181";
  };

  appExtraConfig =
    endpoint: app:
    let
      upstreamHost = app.host or "127.0.0.1";
      authDomain = "auth.${endpoint.base}";
    in
    if app.subdomain == "auth" then
      ''
        reverse_proxy ${endpoint.authProxy}
      ''
    else if (app.protect or false) then
      ''
        forward_auth ${endpoint.authProxy} {
          uri /oauth2/auth
          copy_headers X-Auth-Request-User X-Auth-Request-Email

          @error status 401
          handle_response @error {
            redir * https://${authDomain}/oauth2/start?rd={scheme}://{host}{uri}
          }
        }
        reverse_proxy ${upstreamHost}:${toString app.port}
      ''
    else
      ''
        reverse_proxy ${upstreamHost}:${toString app.port}
      '';

  mkEndpointVhosts =
    endpoint: appList:
    map (app: {
      name = "${app.subdomain}.${endpoint.base}";
      value.extraConfig = appExtraConfig endpoint app;
    }) appList;
in
{
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
      (mkEndpointVhosts localEndpoint apps) ++ (mkEndpointVhosts tailscaleEndpoint apps)
    );
  };

  systemd.services.caddy.serviceConfig.EnvironmentFile = [ "/var/lib/caddy/secrets" ];
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}
