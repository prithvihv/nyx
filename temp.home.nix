{ config, pkgs, ... }:

let 
    xsessionPhv = import ../../nix-config/xsession.nix { inherit pkgs; };
    nvimConfig = import ../../nix-config/dev-tools/nvim/nvim.nix { inherit pkgs; };
    haskellDev = import ../../nix-config/dev-tools/haskell/pkgs.nix { inherit pkgs; };
    alacrittyConfig = with pkgs;import ../../nix-config/alacritty/alacritty.nix { 
      critty = alacritty; 
      inherit fish;
    };
    taffyConfig = import ../../nix-config/taffybar-phv/taffybar.nix { inherit pkgs; };
    vscodium = import ../../nix-config/dev-tools/vscodium/vscodium.nix { inherit pkgs; };
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # get neovim-nightly
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  # BUT WHY?
  home.file.".config/nixpkgs/config.nix".text = ''
  	{ allowUnfree = true; }
  '';

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "phv";
  home.homeDirectory = "/home/phv";

  xsession = xsessionPhv.xsession;

  home.packages = with pkgs;[
  ] 
  ++ xsessionPhv.extraPkgs
  ++ haskellDev.programs
  ++ vscodium.programs
  ++ [
    # for taffy
    # TODO: move this to taffy.nix
    at-spi2-core # https://gist.github.com/jeffcogswell/62395900725acef1c0a5a608f7eb7a05
    hicolor-icon-theme
  ];


  programs.alacritty = alacrittyConfig;
  programs.vscode = vscodium.config;
  programs.neovim = nvimConfig;

  
  # FIXME: enabling is manual right now
  # systemctl --user enable taffybar 
  services.taffybar = taffyConfig;

  # services.polybar = 
  # services.picom = {
  #   enable = true;
  # };
  # services.picom = {
  #   enable = true;
  #   blur = true;
  #   package = pkgs.picom;
  #   # activeOpacity = "1.0";
  #   # inactiveOpacity = "0.8";
  #   backend = "glx";
  #   # fade = true;
  #   # fadeDelta = 5;
  #   # opacityRule = [ "100:name *= 'i3lock'" ];
  #   # shadow = true;
  #   # shadowOpacity = "0.75";
  # };

  # This value determivscodiumnes the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
