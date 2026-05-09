{ pkgs, ... }:

{
  home.stateVersion = "25.11";
  home.homeDirectory = "/var/lib/woodpecker";

  # Tools available to the woodpecker CI agent.
  # Add packages here as needed.
  home.packages = with pkgs; [
    git
  ];
}
