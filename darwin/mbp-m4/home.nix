{ pkgs, lib, ... }:
let
fishConfig = import ../../pkgs/fish.nix { inherit pkgs lib; isWoogaMachine = false; };
in
{
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = fishConfig;

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "25.05";
}
