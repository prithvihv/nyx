{ config, pkgs, lib, ... }:

let
  taffyConfig = import ../../pkgs/taffybar-phv/taffybar.nix { inherit pkgs; };
  dunstConfig = import ../../pkgs/dunst.nix { inherit pkgs; };
  polyBarConfig = import ../../pkgs/polybar/config.nix { inherit pkgs; };
  xsessionPhv = import ./xsession.nix {
    inherit pkgs;
    inherit lib;
    launchPolybar = polyBarConfig.launchPolybar;
  };

  alacrittyConfig = import ../../pkgs/alacritty.nix { };
  fishConfig = import ../../pkgs/fish.nix {
    inherit pkgs;
    inherit lib;
    inherit gzpPrivateStuff;
  };
  tmuxConfig = import ../../pkgs/tmux.nix { inherit pkgs; };

  golangTools = import ../../pkgs/languages/golang.nix { inherit pkgs; };
  haskellTools = import ../../pkgs/languages/haskell.nix { inherit pkgs; };
  clojureTools = import ../../pkgs/languages/clojure.nix { inherit pkgs; };
  nodeTools = import ../../pkgs/languages/node.nix { inherit pkgs; };
  elixirTools = import ../../pkgs/languages/elixir.nix { inherit pkgs; };
  rustTools = import ../../pkgs/languages/rust.nix { inherit pkgs; };
  solanaTools = pkgs.callPackage ../../pkgs/blockchain/solana.nix { };

  # configs
  gzpPrivateStuff = import ../../priv/gzp-stuff.nix {
    inherit config;
    inherit lib;
  };
  vsCodeConfig = import ../../pkgs/vscode.nix {
    inherit gzpPrivateStuff;
    inherit pkgs;
  };
  configPassStore = "/home/phv/.password-store";

  # scripts
  bashidsScript = pkgs.callPackage ./scripts/bashids.nix { };
  rofiBluetooth = pkgs.callPackage ./scripts/rofi-bluetooth.nix { };
  # rofi-with-plugins = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "phv";
  home.homeDirectory = "/home/phv";

  # FIXME: memory leak :/
  #  services.taffybar = taffyConfig;
  # service.polybar = {
  #   enable = true;
  # };

  home.packages = with pkgs;
    [ gnumake nix-diff nixfmt any-nix-shell ] ++ [ # comms
      slack
      discord
      vlc

      gimp
      wakatime

      pciutils
      rsync
      glxinfo
      glances
      inetutils
      jq
      qbittorrent
      imagemagick # convert images
      # example convert -scale 2560x1440 source-image.jpg lockscreen.png
      ffmpeg
      dolphin # file manager
      krusader # file manager

      # krusader dependencies
      kdiff3
      kompare
      xxdiff
      thunderbird
      kmail
      krename
      kate

      age
      sops
      git-crypt
      authy

      brave

      logseq

      redis

      nodePackages.serve
      openjdk17

      fd
      peek

      # solana
      rofimoji
      rofiBluetooth
      gucharmap # find emojis

      calibre

      okular

      unzip
      spotify

      k6
      # github-cli

      pandoc
      # latex stuff
      texlive.combined.scheme-medium
      graphviz # pprof golang needs this
      mypaint

      # jenkins stuff
      # haskellPackages.jenkinsPlugins2nix

      gnome.simple-scan
    ] ++ [ # dev applications
      postman
      lens
      octant
      awscli2
      terraform
      jetbrains.datagrip
      jetbrains.goland
      # jetbrains.idea-ultimate
      redis-desktop-manager
      qt5Full
    ] ++ [ # bash scripts
      bashidsScript
    ] ++ xsessionPhv.extraPkgs ++ golangTools.extraPkgs
    ++ haskellTools.extraPkgs ++ clojureTools.extraPkgs ++ nodeTools.extraPkgs
    ++ elixirTools.extraPkgs ++ rustTools.extraPkgs ++ solanaTools.tools
    ++ polyBarConfig.extraPkgs;

  programs.git = {
    enable = true;
    userName = "prithvihv";
    userEmail = "hvprithvi09@gmail.com";
    extraConfig = {
      # TODO: this doesnt work right now
      user.signingkey = "0x79C7BE63C93CC999";
      # url = { "git@github.com:" = { insteadOf = "https://github.com/"; }; };
      # url = { "https://github.com/" = { insteadOf = "git@github.com:"; }; };
    };
  };

  programs.autorandr = xsessionPhv.autorandr;

  programs.command-not-found = { enable = true; };

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

  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock
      "bkhaagjahfmjljalopjnoealnfndnagc" # octotree
      "omdakjcmkglenbhjadbccaookpfjihpa" # tunnel bear
      "gbmdgpbipfallnflgajpaliibnhdgobh" # json viewer
    ];
  };

  programs.ssh = {
    enable = true;
    matchBlocks = { } // gzpPrivateStuff.gzp-ssh;
  };

  programs.gpg = { enable = true; };

  programs.obs-studio.enable = true;

  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "qt";
  };

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
  xsession = xsessionPhv.xsession;

  services.picom = {
    enable = true;
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
    # package = 
    client = { enable = true; };
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
