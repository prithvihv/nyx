{ config, pkgs, ... }:

let 
    xsessionPhv = import ../../nyx/xsession.nix { inherit pkgs; };
    nvimConfig = import ../../nyx/dev-tools/nvim/nvim.nix { inherit pkgs; };
    haskellDev = import ../../nyx/dev-tools/haskell/pkgs.nix { inherit pkgs; };
    alacrittyConfig = with pkgs;import ../../nyx/alacritty/alacritty.nix { 
      critty = alacritty; 
      inherit fish;
    };
    taffyConfig = import ../../nyx/taffybar-phv/taffybar.nix { inherit pkgs; };
    gitConfig = pkgs.callPackages ../../nyx/dev-tools/git.nix {  };
    vscodium = import ../../nyx/dev-tools/vscodium/vscodium.nix { inherit pkgs; };
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
  };

  # get neovim-nightly
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];


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
    # hicolor-icon-theme

    jetbrains-mono
  ];


  programs.alacritty = alacrittyConfig;
  programs.vscode = vscodium.config;
  programs.neovim = nvimConfig;
  programs.git = gitConfig;

  
  # FIXME: enabling is manual right now
  # systemctl --user enable taffybar 
  services.taffybar = taffyConfig;

  # services.polybar = 
  # services.picom = {
  #   enable = true;
  # };
  # FIXME: enabling is mnaual
  # systemctl --user start picom.service
  # systemctl --user enable picom.service
  services.picom = {
    enable = true;
    blur = true;
    package = pkgs.picom;
    # activeOpacity = "1.0";
    # inactiveOpacity = "0.8";
    backend = "glx";
    # fade = true;
    # fadeDelta = 5;
    # opacityRule = [ "100:name *= 'i3lock'" ];
    # shadow = true;
    # shadowOpacity = "0.75";
  };

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
