{ config, pkgs, ... }:

{
  services.home-assistant = {
    enable = true;
    openFirewall = true;

    extraComponents = [
      "tplink"
      "met"
      "radio_browser"
    ];

    config = {
      homeassistant = {
        name        = "Home";
        unit_system = "metric";
        time_zone   = "Europe/Berlin";
        currency    = "EUR";
      };
      default_config = {};
    };
  };
}
