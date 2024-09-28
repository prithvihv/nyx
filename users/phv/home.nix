{ pkgs, config, lib, osConfig, ... }:

let
  nixosSystemBuild = osConfig.nixos-system-build;
  dunstConfig = import ../../pkgs/dunst.nix { inherit pkgs; };
  polyBarConfig = import ../../pkgs/polybar/config.nix { inherit pkgs; };
  xsessionPhv = import ./xsession.nix {
    inherit pkgs;
    inherit lib;
    inherit nixosSystemBuild;
    launchPolybar = polyBarConfig.launchPolybar;
  };

  alacrittyConfig = import ../../pkgs/alacritty.nix { };
  fishConfig = import ../../pkgs/fish.nix {
    inherit pkgs;
    inherit lib;
  };
  tmuxConfig = import ../../pkgs/tmux.nix { inherit pkgs; };
  vimConfig = import ../../pkgs/vim.nix { inherit pkgs; };

  golangTools = import ../../pkgs/languages/golang.nix { inherit pkgs lib; };
  haskellTools = import ../../pkgs/languages/haskell.nix { inherit pkgs; };
  clojureTools = import ../../pkgs/languages/clojure.nix { inherit pkgs; };
  nodeTools = import ../../pkgs/languages/node/node.nix {
    inherit pkgs;
    includePrismaTools = true;
  };
  elixirTools = import ../../pkgs/languages/elixir.nix { inherit pkgs; };
  rustTools = import ../../pkgs/languages/rust.nix { inherit pkgs; };
  solanaTools = pkgs.callPackage ../../pkgs/blockchain/solana.nix { };

  # configs
  vsCodeConfig = import ../../pkgs/vscode.nix { inherit pkgs; };
  gitConfig = import ../../pkgs/git.nix { inherit pkgs; };
  configPassStore = "/home/phv/.password-store";

  # scripts
  bashidsScript = pkgs.callPackage ./scripts/bashids.nix { };
  rofiBluetooth = pkgs.callPackage ./scripts/rofi-bluetooth.nix { };

  # latex stuff
  tex = with pkgs;
    (texlive.combine {
      inherit (texlive)
        scheme-medium titlesec marvosym xcolor enumitem hyperref fancyhdr
        latexmk tlmgrbasics fontawesome;
    });
  # rofi-with-plugins = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
  customPkgs = import ../../pkgs/nixpkgs { inherit pkgs; };
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "phv";
  home.homeDirectory = "/home/phv";

  gtk = let common-extra-gtk = { gtk-application-prefer-dark-theme = 1; };
  in {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    theme = {
      name = "Flat-Remix-GTK-Blue-Darkest";
      package = pkgs.flat-remix-gtk;
    };
    cursorTheme = {
      package = pkgs.cinnamon.mint-cursor-themes;
      # https://github.com/linuxmint/mint-cursor-themes/tree/master/usr/share/icons
      # if you need to switch it up, use the above link
      name = "Bibata-Modern-Classic";
    };
    font = { name = "Iosevka 13"; };
    gtk3 = {
      bookmarks = [ "file:///home/phv/code Code" "file:///home/phv/code/phv/resume/cuts Resume-Cuts" ];
      extraConfig = common-extra-gtk;
    };
    # gtk2.extraConfig = common-extra-gtk;
    gtk4.extraConfig = common-extra-gtk;
  };
  home.pointerCursor = {
    name = "Bibata-Modern-Classic";
    package = pkgs.cinnamon.mint-cursor-themes;
    size = 42;
    x11 = {
      enable = true;
      defaultCursor = "Bibata-Modern-Classic";
    };
  };

  home.sessionVariables = { GTK_THEME = "Adwaita:dark"; };

  home.packages = with pkgs;
    [ gnumake nix-diff nixfmt any-nix-shell ] ++ [

      # gui applications
      slack
      discord
      vlc
      logseq
      gimp
      calibre
      okular
      spotify
      qbittorrent
      brave
      peek

      anki
      ocrmypdf
      zathura

      libreoffice

      # these are broken but i want them
      protonvpn-gui
      protonvpn-cli

      # encrypt
      age
      sops
      git-crypt

      redis

      # java stuff
      openjdk17
      # gradle

      # TODO: these don't work with dpi, need to switch to use command like initiation 
      rofimoji
      rofiBluetooth

      android-file-transfer
    ] ++ [ # linux software terminal / cli / system
      fd
      jq
      unzip
      duf
      nodePackages.serve
      imagemagick # convert images
      # example convert -scale 2560x1440 source-image.jpg lockscreen.png
      ffmpeg
      inetutils # telnet
      cryptsetup # LUKS
      hdparm # set drive parameters
      parted
      upower
      gptfdisk
      dig
      simple-scan

      # docs content files
      # https://pandoc.org/MANUAL.html
      pandoc
      pdftk
      tex
      mypaint

      gucharmap # find emojis
      xorg.xev

      pciutils
      rsync
      glxinfo
      glances

      rclone

      gnome.sushi
      gnome.nautilus
      # cinnamon.nemo
      # libsForQt5.dolphin
      # pcmanfm
      # lxqt.pcmanfm-qt

      gtk3
      # printer
      # canon-cups-ufr2
      gnomeExtensions.printers
    ] ++ [ # dev applications
      # cli: external
      awscli2
      github-cli
      go-migrate
      pgsync
      terraform
      wakatime
      gitleaks
      unp # "unpack (almost) everything with one command"
      unrar
      mtpfs # trying to get kindle working
      gmtp
      gparted
      signal-desktop
      powertop

      neovide

      # gui
      # postman # TODO: not working right now
      lens
      jetbrains.datagrip

      # dual audio playback
      paprefs # pulse audio preference
      dconf
    ] ++ [ # bash scripts
      bashidsScript
    ] ++ [ # yubikey
      yubikey-manager # cli
      yubikey-manager-qt # gui
    ] ++ xsessionPhv.extraPkgs ++ golangTools.extraPkgs
    ++ haskellTools.extraPkgs ++ clojureTools.extraPkgs ++ nodeTools.extraPkgs
    ++ elixirTools.extraPkgs ++ rustTools.extraPkgs
    # ++ solanaTools.tools
    ++ polyBarConfig.extraPkgs ++ customPkgs.all;

  programs.git = gitConfig;

  services.dropbox = { enable = true; };

  xdg.mimeApps = let
    mimeMapping = {
      # https://discourse.nixos.org/t/set-default-application-for-mime-type-with-home-manager/17190/2
      "application/pdf" = [ "code.desktop" ];
      "inode/directory" = [ "org.gnome.Nautilus.desktop" ];

      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "x-scheme-handler/chrome" = [ "firefox.desktop" ];
      "text/html" = [ "firefox.desktop" ];
      "application/x-extension-htm" = [ "firefox.desktop" ];
      "application/x-extension-html" = [ "firefox.desktop" ];
      "application/x-extension-shtml" = [ "firefox.desktop" ];
      "application/xhtml+xml" = [ "firefox.desktop" ];
      "application/x-extension-xhtml" = [ "firefox.desktop" ];
      "application/x-extension-xht" = [ "firefox.desktop" ];
    };
  in {
    enable = true;
    defaultApplications = mimeMapping;
    associations.added = mimeMapping;
  };

  programs.autorandr = xsessionPhv.autorandr;

  programs.nix-index = {
    enable = true;
    enableFishIntegration = true;
  };

  # explore stuff here https://www.freecodecamp.org/news/fzf-a-command-line-fuzzy-finder-missing-demo-a7de312403ff/
  # https://pragmaticpineapple.com/four-useful-fzf-tricks-for-your-terminal/
  programs.fzf = {
    enable = true;
    tmux.enableShellIntegration = true;
    enableFishIntegration = true;
    changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type directory"; # ALT-C
    historyWidgetOptions = [ ];
    # TODO: terminal auto complete with ** 
    # TODO: 
  };

  # programs.go = {
  #   enable = true;
  #   package = pkgs.go_1_17;
  #   goPath = "code";
  #   goPrivate = ["github.com/gamezop"];
  # };

  programs.htop = {
    enable = true;
    settings = { color_scheme = 6; };
  };

  programs.firefox = { enable = true; };
  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock
      "omdakjcmkglenbhjadbccaookpfjihpa" # tunnel bear
      "gbmdgpbipfallnflgajpaliibnhdgobh" # json viewer
    ];
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github-wooga.com" = {
        hostname = "github.com";
        user = "prithvihv-wooga";
        identityFile = "/home/phv/.ssh/phv-wooga-macbook/id_rsa";
      };
    };
  };

  programs.obs-studio.enable = true;
  programs.bat.enable = true;

  programs.gpg = { enable = true; };

  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-qt;
    # pinentryFlavor = "qt";
  };

  # TODO: figur out dropbox configuration
  # services.dropbox = { enable = true; };

  programs.password-store = {
    enable = true;
    settings = { PASSWORD_STORE_DIR = configPassStore; };
  };

  programs.rofi = {
    enable = true;
    pass = {
      enable = true;
      stores = [ configPassStore ];
    };
    # theme = "DarkBlue";
    # reference: https://raw.githubusercontent.com/dracula/rofi/master/theme/config2.rasi
    theme = ../../pkgs/rofi/theme-dracula.rasi;
    # package = pkgs.rofi;

    # FIXME this doesnt work
    # plugins = [ pkgs.rofi-emoji ];
    # package = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ];  };
  };

  programs.vscode = vsCodeConfig;
  programs.alacritty = alacrittyConfig;
  programs.fish = fishConfig;
  programs.tmux = tmuxConfig;
  programs.neovim = vimConfig;
  xsession = xsessionPhv.xsession;

  services.picom = {
    enable = false;
    fade = true;
    fadeSteps = [ "0.03" "0.03" ];
    fadeDelta = 1;
    # opacityRule = [ "100:name *= 'i3lock'" ];
  };
  services.udiskie.enable = true;

  # consider ddead-notification in haskell
  services.dunst = dunstConfig;

  # uses upower
  services.poweralertd.enable = true;
  services.blueman-applet.enable = true;

  services.clipmenu.enable = true;

  services.polybar = polyBarConfig.home-manager-config;

  services.emacs = {
    enable = true;
    client = { enable = true; };
  };

  services.xscreensaver = {
    enable = true;
    settings = {
      mode = "blank";
      lock = false;
      # fadeTick = 20;
      timeout = "00:00:20";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
