{ config, pkgs, ... }:

let
  taffyConfig = import ../../pkgs/taffybar-phv/taffybar.nix { inherit pkgs; };
  dunstConfig = import ../../pkgs/dunst.nix { inherit pkgs; };
  xsessionPhv = import ./xsession.nix { inherit pkgs; };

  alacrittyConfig = import ../../pkgs/alacritty.nix { };
  vsCodeConfig = import ../../pkgs/vscode.nix { inherit pkgs; };
  fishConfig = import ../../pkgs/fish.nix { inherit pkgs; };
  tmuxConfig = import ../../pkgs/tmux.nix { inherit pkgs; };
  golangTools = import ../../pkgs/languages/golang.nix { inherit pkgs; };

  # configs
  configPassStore = "/home/phv/.password-store";

in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "phv";
  home.homeDirectory = "/home/phv";

  # This will, for example, allow fontconfig to discover fonts and configurations installed through home.packages and nix-env. 
  fonts.fontconfig.enable = true;

  # FIXME: enabling is manual right now
  # systemctl --user enable taffybar 
  services.taffybar = taffyConfig;

  home.packages = with pkgs;
    [ taffyConfig.package gnumake nix-diff nixfmt ] ++ [ # comms
      slack
      discord
    ] ++ [ # fonts
      jetbrains-mono
    ] ++ [ postman ] ++ xsessionPhv.extraPkgs ++ golangTools.extraPkgs;

  programs.git = {
    enable = true;
    userName = "prithvihv";
    userEmail = "hvprithvi09@gmail.com";
    extraConfig = {
      url = { "git@github.com:" = { insteadOf = "https://github.com/"; }; };
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
    settings = {
      color_scheme = 6;
    };
  };

  programs.chromium = {
    enable = true;
    extensions = [
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock
      "bkhaagjahfmjljalopjnoealnfndnagc" # octotree
      "omdakjcmkglenbhjadbccaookpfjihpa" # tunnel bear`
    ];
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "gz_jump" = {
        hostname = "13.234.205.34";
        user = "prithvi";
        identityFile = "/home/phv/.keybox/.ssh/skadi/id_rsa";
      };
    };
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
  };

  programs.vscode = vsCodeConfig;
  programs.alacritty = alacrittyConfig;
  programs.fish = fishConfig;
  programs.tmux = tmuxConfig;
  xsession = xsessionPhv.xsession;

  services.picom = { enable = true; };
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
