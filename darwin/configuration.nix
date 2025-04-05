{ config, pkgs, lib, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ nixfmt ];
  # nixpkgs.config.allowUnsupportedSystem = true;
  # nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (pkgs.lib.getName pkg) [ "vscode" ];

  # TODO: this does not work, need to run command manually:
  # chsh -s /etc/profiles/per-user/prithvi.virupaksha/bin/fish
  environment.shells = [ "/etc/profiles/per-user/prithvi.virupaksha/bin/fish" ];

  services.postgresql.enable = true;
  users.users."prithvi.virupaksha" = {
    name = "prithvi.virupaksha";
    home = "/Users/prithvi.virupaksha";
    shell = "/etc/profiles/per-user/prithvi.virupaksha/bin/fish";
  };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  nix.extraOptions = ''
    extra-platforms = aarch64-darwin x86_64-darwin
    experimental-features = nix-command flakes
  '';

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  programs.fish.enable = true;
  programs.bash.enable = true;

  system.stateVersion = 5;
}
