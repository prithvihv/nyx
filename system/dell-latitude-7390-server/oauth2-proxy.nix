{ ... }:

{
  services.oauth2-proxy = {
    enable = true;
    provider = "google";
    keyFile = "/var/lib/oauth2-proxy/secrets.env";

    email.addresses = ''
      hvprithvi09@gmail.com
    '';

    reverseProxy = true;
    trustedProxyIP = [
      "127.0.0.1/32"
      "::1/128"
    ];
    setXauthrequest = true;
    redirectURL = "https://auth.local.prithvihv.xyz/oauth2/callback";
    cookie.domain = ".local.prithvihv.xyz";

    # Used purely as an auth endpoint; it has nothing of its own to proxy.
    upstream = [ "static://202" ];

    # Allow post-login redirects back to any service subdomain (SSO).
    extraConfig.whitelist-domain = ".local.prithvihv.xyz";
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/oauth2-proxy 0750 root root -"
  ];
}
