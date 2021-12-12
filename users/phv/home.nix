{ config, pkgs, ... }:

let
  alacrittyConfig = import ../../pkgs/alacritty.nix { };
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "phv";
  home.homeDirectory = "/home/phv";

  # This will, for example, allow fontconfig to discover fonts and configurations installed through home.packages and nix-env. 
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    xclip
    pinentry_qt
  ] ++ [ # comms
    slack
    discord
  ] ++ [ # fonts
    jetbrains-mono
  ];

  programs.git = {
    enable = true;
    userName = "prithvihv";
    userEmail = "hvprithvi09@gmail.com";
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

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "qt";
  };
  
  programs.password-store = {
    enable = true;
  };
  
  programs.rofi = {
   enable = true;
   pass = {
     enable = true;
     stores = [
       "/home/phv/.password-store"
     ];
   };
  };
  
  programs.vscode = {
    enable = true;
  };
  programs.alacritty = alacrittyConfig;

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
