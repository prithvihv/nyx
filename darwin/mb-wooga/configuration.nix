{ config, pkgs, lib, ... }:

{
  ids.gids.nixbld = 30000;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ nixfmt ];
  # nixpkgs.config.allowUnsupportedSystem = true;
  # nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (pkgs.lib.getName pkg) [ "vscode" ];

  # TODO: this does not work, need to run command manually:
  # chsh -s /etc/profiles/per-user/prithvi.virupaksha/bin/fish
  environment.shells = [ pkgs.fish ];

  services.postgresql.enable = true;
  users.users."prithvi.virupaksha" = {
    name = "prithvi.virupaksha";
    home = "/Users/prithvi.virupaksha";
    # extraGroups = [ "wheel"];
  };
  system.primaryUser = "prithvi.virupaksha";

  programs.fish.enable = true;

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  nix.enable = true;
  nix.package = pkgs.nix;
  # nix.package = pkgs.lixPackageSets.latest.lix;
  nix.extraOptions = ''
    extra-platforms = aarch64-darwin x86_64-darwin
    experimental-features = nix-command flakes
  '';

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 5;
}
