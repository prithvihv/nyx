{
  pkgs,
  config,
  lib,
  ...
}:
let
  fishConfig = import ../../pkgs/fish.nix {
    inherit pkgs;
    inherit lib;
    isWoogaMachine = false;
  };
  tmuxConfig = import ../../pkgs/tmux.nix { inherit pkgs; };
  gitConfig = import ../../pkgs/git.nix { inherit pkgs; };
in
{
  home.username = "server";
  home.homeDirectory = "/home/server";

  home.packages = with pkgs; [

  ];

  programs.htop = {
    enable = true;
    settings = {
      color_scheme = 6;
    };
  };

  programs.fish = fishConfig;
  programs.tmux = tmuxConfig;
  programs.git = gitConfig;

  programs.nix-index = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.fzf = {
    enable = true;
    tmux.enableShellIntegration = true;
    enableFishIntegration = true;
    changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type directory"; # ALT-C
    historyWidgetOptions = [ ];
    # TODO: terminal auto complete with **
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
