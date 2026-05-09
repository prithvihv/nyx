{ config, pkgs, ... }:

{
  services.home-assistant = {
    enable = true;
    openFirewall = true;

    extraComponents = [
      # TP-Link Smart Home — covers Tapo devices
      "tplink"

      # Useful defaults
      "met"           # weather
      "radio_browser" # internet radio
    ];

    config = {
      homeassistant = {
        name        = "Home";
        unit_system = "metric";
        time_zone   = "Europe/Berlin";
        currency    = "EUR";
      };

      # Enable the default dashboard and built-in integrations
      default_config = {};

      # Lovelace UI in auto mode (managed via UI)
      lovelace.mode = "auto";
    };
  };
}
