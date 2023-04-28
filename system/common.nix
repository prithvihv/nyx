{ config, lib, pkgs, modulesPath, ... }: { # nix flakes
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };
  # https://wiki.archlinux.org/title/Fcitx5
  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = with pkgs; [
    # once mozc is added open the GUI and add it to defaults. CTRL + SPACE is to switch between defaults group
    fcitx5-mozc # japanese
    fcitx5-gtk
    # fcitx5-rime # traditional chinese
    # fcitx5-table-other
    # fcitx5-configtool
    # fcitx5-m17n
  ];

  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      hasklig
      iosevka
      source-code-pro

      # TODO: this broke in 22.11
      # noto-fonts
      nerdfonts
      # ubuntu_font_family
      unifont
      jetbrains-mono
      font-awesome
      font-awesome_5
      cantarell-fonts
      siji
      dejavu_fonts
      material-icons
      cantarell-fonts

      # japanese
      ipafont
      kochi-substitute
    ];
    fontconfig = {
      enable = true;

      # might need to enable this to make it look nicer
      # ultimate.enable = true;

      # might need to set up default fonts
      # defaultFonts = {};
    };
  };

  # Enable the GNOME Desktop Environment.
  services.xserver = {
    enable = true;
    # dpi = 196;
    # videoDrivers = [ "nvidia" ];
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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.phv = {
    isNormalUser = true;
    hashedPassword =
      "$6$iA.Ln4D87zK1nWpa$tS7r6fQE3a7kQs0PgAaO5UntgHRHB9c9GQ2Dw1LkqSDLD8Buv2Bs4Hdf3XmpS0HmGEhKC.A6YIIQ00AMUbUwr1";
    extraGroups =
      [ "wheel" "networkmanager" "lxd" "docker" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # postgres
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

  services.hardware.bolt.enable = true;
}
