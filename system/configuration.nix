# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

inputs@{ lib, config, pkgs, ... }:

let
  gzp-vpn = import ./../priv/gzp-stuff.nix { inherit config; };
  hroneTokenScript = import ./../priv/hrone.token.nix { inherit pkgs; };
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # nix flakes
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "electron-9.4.4"
  ];

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraConfig = ''
    phv ALL=(ALL) NOPASSWD: ALL
  '';

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nyx"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp59s0.useDHCP = true;
  networking.wg-quick.interfaces = { gzp-dev = gzp-vpn.gzp-dev; };

  # Secret management
  sops = {
    defaultSopsFile = ../sops/secrets/nyx.yaml;
    age.keyFile = "/home/phv/.keybox/age.sops.txt";
    secrets = let
      secrets = [
        "vpn/gzp_dev/private"
        "secret_name" # need some defaults, for nested paths to work
      ];
      permissionSettings = {
        mode = "0440";
        owner = config.users.users.phv.name;
        group = config.users.users.phv.group;
      };
      defaultPermissions = secret: { ${secret} = permissionSettings; };
    in lib.foldl' lib.mergeAttrs { } (builtins.map defaultPermissions
      secrets); # building map with keys of secret paths
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.

  # Enable the GNOME Desktop Environment.
  # services.xserver.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;
  services.xserver = {
    enable = true;
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };
    displayManager = {
      defaultSession = "xsession";
      lightdm.enable = true;
      session = [{
        manage = "desktop";
        name = "xsession";
        start = "exec $HOME/.xsession";
      }];
    };
  };
  services.cron = {
    enable = true;
    # FIXME: add shellscripts to nixstore
    systemCronJobs = [
      "0 10,11,20,22 * * *      phv    /home/phv/.nyx/users/phv/cron/hrone.sh"
      "0 9 * * *        phv  ${hroneTokenScript}/bin/hrone_token"
      "*/10 * * * *  phv  /home/phv/.nyx/users/phv/cron/killtaffy.sh"
    ];
  };
  services.upower.enable = true;
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host    all             all             127.0.0.1/32            trust
      host    all             all             0.0.0.0/0            md5
      host    all             all             localhost            trust
    '';
  };

  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.phv = {
    isNormalUser = true;
    # initialPassword = "qwerty123";
    hashedPassword =
      "$6$iA.Ln4D87zK1nWpa$tS7r6fQE3a7kQs0PgAaO5UntgHRHB9c9GQ2Dw1LkqSDLD8Buv2Bs4Hdf3XmpS0HmGEhKC.A6YIIQ00AMUbUwr1";
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
      "keys"
    ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    vim
    git
    hello
    firefox
  ];
  virtualisation.docker.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

