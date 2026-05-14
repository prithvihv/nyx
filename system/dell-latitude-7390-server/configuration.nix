# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ../builds.module.nix
    ../dell-latitude-7390/hardware-configuration.nix
    ./vars.nix
    ./power.nix
    ./woodpecker.nix
    ./home-assistant.nix
    ./ingress.nix
  ];

  nix.package = pkgs.nixVersions.stable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.hostName = "Dell-Latitude-7390"; # Define your hostname.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.
  networking.resolvconf.enable = false;
  networking.networkmanager.insertNameservers = [ "192.168.0.51" ];

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  programs.fish.enable = true;

  users.users.server = {
    isNormalUser = true;
    hashedPassword = "$6$iA.Ln4D87zK1nWpa$tS7r6fQE3a7kQs0PgAaO5UntgHRHB9c9GQ2Dw1LkqSDLD8Buv2Bs4Hdf3XmpS0HmGEhKC.A6YIIQ00AMUbUwr1";
    description = "server user";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFU/2xyaZ7ibaJCWKzNMnVG8XytT/p1l4Y4KvNZ0PukH prithvi.virupaksha@mw-pvirupa-GK4K"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHXJuhA4KlC9bZfO1lSktbnTK2ca5ZDkGWAXompXPFjw phv@m4-mbp14.local"
    ];
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  services.printing.enable = false;

  # Enable sound.
  # sound.enable = false;
  services.pulseaudio.enable = false;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  services.forgejo = {
      enable = true;

      lfs.enable = true;

      settings = {
        server = {
          DOMAIN = "git.local.prithvihv.xyz";
          ROOT_URL = "https://git.local.prithvihv.xyz/";
          HTTP_ADDR = "0.0.0.0";
          HTTP_PORT = 9753;

          SSH_DOMAIN = "192.168.0.51";
          SSH_PORT = 2222;

          LOCAL_ROOT_URL = "http://127.0.0.1:9753/";

          START_SSH_SERVER = true;
          SSH_LISTEN_HOST = "0.0.0.0";
          SSH_LISTEN_PORT = 2222;
        };

        actions.ENABLED = false;

        webhook.ALLOWED_HOST_LIST = "loopback,private";
      };
    };



  # Enable Pi-hole FTL (DNS server)
  services.pihole-ftl = {
    enable = true;
    openFirewallDNS = true;
    openFirewallWebserver = true;
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
    ports = [ 4938 ];         # match webserver.port above
    # optional, default is "pi.hole"
    # hostName = "pi.hole";
  };

  # # for a WiFi printer
  # services.avahi.enable = true;
  # services.avahi.nssmdns = true;
  # services.avahi.openFirewall = true;
  services.avahi.enable = false;

  services.upower.enable = true;

  # Keep running when the lid is closed
  services.logind.settings.Login.HandleLidSwitch = "ignore";
  services.logind.settings.Login.HandleLidSwitchExternalPower = "ignore";

  # Turn off backlight on lid close, restore on open
  services.acpid = {
    enable = true;
    handlers.lid-close = {
      event = "button/lid LID close";
      action = "echo 0 > /sys/class/backlight/intel_backlight/brightness";
    };
    handlers.lid-open = {
      event = "button/lid LID open";
      action = "echo 1500 > /sys/class/backlight/intel_backlight/brightness";
    };
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 9753 2222 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  nixos-system-build =
    let
      builds = import ../builds.nix;
    in
    builds.dell-latitude-7390;
}
