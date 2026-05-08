{ config, pkgs, ... }:

{
  services.tlp = {
    enable = true;
    settings = {
      # CPU governor - prefer low clocks when idle
      CPU_SCALING_GOVERNOR_ON_AC = "powersave";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      # Disable turbo boost - not needed for a mostly-idle server
      CPU_BOOST_ON_AC = 0;
      CPU_BOOST_ON_BAT = 0;

      # WiFi power saving OFF - needed for smart devices on the network
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "off";

      # NVMe/SATA drive power management
      AHCI_RUNTIME_PM_ON_AC = "auto";
      AHCI_RUNTIME_PM_ON_BAT = "auto";

      # PCIe power management
      PCIE_ASPM_ON_AC = "powersupersave";
      PCIE_ASPM_ON_BAT = "powersupersave";
    };
  };
}
