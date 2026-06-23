{ config, pkgs, ... }:

{
  services.home-assistant = {
    enable = true;
    # Loopback/Caddy only (home-assistant.<domain>). Point the HA mobile app at
    # the Caddy URL rather than the raw :8123 port.
    openFirewall = false;

    extraComponents = [
      "tplink"
      "met"
      "radio_browser"
    ];

    config = {
      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = "Europe/Berlin";
        currency = "EUR";
      };
      default_config = { };

      # HA sits behind Caddy (reverse_proxy 127.0.0.1:8123), which adds an
      # X-Forwarded-For header. Without this, HA rejects every proxied request
      # with "400: Bad Request". Trust only the loopback proxy.
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
    };
  };
}
