{ config, pkgs, lib, ... }:

let
  taffyConfig = import ../../pkgs/taffybar-phv/taffybar.nix { inherit pkgs; };
  dunstConfig = import ../../pkgs/dunst.nix { inherit pkgs; };
  xsessionPhv = import ./xsession.nix { inherit pkgs; };

  alacrittyConfig = import ../../pkgs/alacritty.nix { };
  vsCodeConfig = import ../../pkgs/vscode.nix { inherit pkgs; };
  fishConfig = import ../../pkgs/fish.nix {
    inherit pkgs;
    inherit gzpPrivateStuff;
  };
  tmuxConfig = import ../../pkgs/tmux.nix { inherit pkgs; };
  polyBarConfig = import ../../pkgs/polyBarConfig { inherit pkgs; };

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
  configPassStore = "/home/phv/.password-store";

  # scripts
  bashidsScript = pkgs.callPackage ./scripts/bashids.nix { };
  # rofi-with-plugins = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "phv";
  home.homeDirectory = "/home/phv";

  # This will, for example, allow fontconfig to discover fonts and configurations installed through home.packages and nix-env. 
  fonts.fontconfig.enable = true;

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

      pciutils
      glxinfo
      glances
      telnet
      jq
      qbittorrent
      imagemagick # convert images
      # example convert -scale 2560x1440 source-image.jpg lockscreen.png

      age
      sops
      git-crypt
      authy

      redis

      nodePackages.serve

      # solana

      emacs
      rofimoji

      # rofi.override { plugins = [ rofi-emoji ]; }
      calibre

      unzip
    ] ++ [ # fonts
      jetbrains-mono
      # rofi-emoji
    ] ++ [ # dev applications
      postman
      lens
      octant
      awscli2
      terraform
      jetbrains.datagrip
      redis-desktop-manager
      qt5Full
      beekeeper-studio
    ] ++ [ # bash scripts
      bashidsScript
    ] ++ xsessionPhv.extraPkgs ++ golangTools.extraPkgs
    ++ haskellTools.extraPkgs ++ clojureTools.extraPkgs ++ nodeTools.extraPkgs
    ++ elixirTools.extraPkgs ++ rustTools.extraPkgs ++ solanaTools.tools;

  programs.git = {
    enable = true;
    userName = "prithvihv";
    userEmail = "hvprithvi09@gmail.com";
    extraConfig = {
      user.signingkey = "0x79C7BE63C93CC999";
      url = { "git@github.com:" = { insteadOf = "https://github.com/"; }; };
      # url = { "https://github.com/" = { insteadOf = "git@github.com:"; }; };
    };
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
    theme = "DarkBlue";
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
    # opacityRule = [ "100:name *= 'i3lock'" ];
  };
  services.udiskie.enable = true;

  # consider ddead-notification in haskell
  services.dunst = dunstConfig;

  # uses upower
  services.poweralertd.enable = true;

  services.clipmenu.enable = true;

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
