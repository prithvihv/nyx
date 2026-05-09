{ pkgs, ... }:

{
  home.stateVersion = "25.11";
  home.homeDirectory = "/var/empty";

  # Tools available to the woodpecker CI agent.
  # Add packages here as needed.
  home.packages = with pkgs; [
    git
  ];
}
