{ config, pkgs, ... }:

{
  # Enable Pi-hole FTL (DNS server)
  services.pihole-ftl = {
    enable = true;
    openFirewallDNS = true;
    # Web UI stays on loopback; reach it through Caddy (pihole.<domain>), which
    # gates it behind oauth2-proxy.
    openFirewallWebserver = false;
    settings = {
      dns = {
        upstreams = [
          "1.1.1.1"
          "1.0.0.1"
          "8.8.8.8"
          "8.8.4.4"
        ];
      };
      #"webserver.paths" = {
      # webroot = "${pkgs.pihole-web}/share";
      #};
      webserver = {
        port = "4938";
        serve_all = true;
      };
      misc = {
        privacylevel = 0;
      };
      ntp.sync.server.active = false;
    };
    lists = [
      { url = "https://cdn.jsdelivr.net/gh/hagezi/dns-blocklists@latest/adblock/pro.txt"; }
    ];
  };

  services.pihole-web = {
    enable = true;
    ports = [ 4938 ]; # match webserver.port above
    # optional, default is "pi.hole"
    # hostName = "pi.hole";
  };
}
