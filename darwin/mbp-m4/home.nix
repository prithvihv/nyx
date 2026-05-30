{ pkgs, lib, ... }:
let
  fishConfig = import ../../pkgs/fish.nix {
    inherit pkgs lib;
    isWoogaMachine = false;
  };
  gitConfig = import ../../pkgs/git.nix { inherit pkgs; };
in
{
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = fishConfig;
  programs.git = gitConfig;

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    qtpass
    pass
    gnutar
    gnupg # does not show up in Application dir, currently openning it manually
    nixd # nix language server
    nil # nix language server
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "25.05";
}
