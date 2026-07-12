{ config, ... }:

let
  domain = config.local.domain;
  tailscaleDomain = config.local.tailscaleDomain;

  mkOauth2Proxy =
    {
      base,
      keyFile,
      httpAddress,
      cookieName,
    }:
    {
      enable = true;
      provider = "google";
      inherit keyFile httpAddress;

      email.addresses = ''
        hvprithvi09@gmail.com
      '';

      reverseProxy = true;
      trustedProxyIP = [
        "127.0.0.1/32"
        "::1/128"
      ];
      setXauthrequest = true;

      # Used purely as an auth endpoint; it has nothing of its own to proxy.
      upstream = [ "static://202" ];

      redirectURL = "https://auth.${base}/oauth2/callback";

      # Allow post-login redirects back to any service subdomain (SSO).
      extraConfig.whitelist-domain = ".${base}";

      cookie = {
        domain = ".${base}";
        name = cookieName;
      };
    };
  oauth2-proxy = mkOauth2Proxy {
    base = domain;
    keyFile = "/var/lib/oauth2-proxy/secrets.env";
    httpAddress = "http://127.0.0.1:4180";
    cookieName = "_oauth2_proxy";
  };
  oauth2-proxy-ts = mkOauth2Proxy {
    base = tailscaleDomain;
    keyFile = "/var/lib/oauth2-proxy-ts/secrets.env";
    httpAddress = "http://127.0.0.1:4181";
    cookieName = "_oauth2_proxy_ts";
  };
in
{

  systemd.tmpfiles.rules = [
    "d /var/lib/oauth2-proxy 0750 root root -"
    "d /var/lib/oauth2-proxy-ts 0700 root root -"
  ];
  
  services.oauth2-proxy = oauth2-proxy;

  # auth for https://auth.tailscale.<domain>/oauth2/callback
  containers.tailscale-oauth-server = {
    autoStart = true;

    bindMounts."/var/lib/oauth2-proxy-ts" = {
      hostPath = "/var/lib/oauth2-proxy-ts";
      isReadOnly = true;
    };

    config =
      { ... }:
      {
        system.stateVersion = "22.11";
        # Shares the host netns, whose firewall already governs the stack; this
        # proxy is loopback-only, so the container manages no rules of its own.
        networking.firewall.enable = false;

        services.oauth2-proxy = oauth2-proxy-ts;
      };
  };
}
