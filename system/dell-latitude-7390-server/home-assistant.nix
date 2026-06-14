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
    };
  };
}
