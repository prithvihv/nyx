{ pkgs, lib, ... }:
let
fishConfig = import ../../pkgs/fish.nix { inherit pkgs lib; isWoogaMachine = false; };
gitConfig = import ../../pkgs/git.nix { inherit pkgs; };
in
{
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = fishConfig;
  programs.git = gitConfig;

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "25.05";
}
