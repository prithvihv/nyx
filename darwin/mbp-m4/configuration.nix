{ self, pkgs, ... }: {
  users.users."phv" = {
    name = "phv";
    home = "/Users/phv";
    shell = pkgs.fish;
    ignoreShellProgramCheck = true;
  };
  system.primaryUser = "phv";
  networking.computerName = "mbp-m4";
  networking.hostName      = "mbp-m4";
  networking.localHostName = "mbp-m4";
  environment.shells = [ pkgs.fish ];
  programs.fish.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    with pkgs; [ vim ];

  fonts.packages = import ../../pkgs/fonts.nix { inherit pkgs; };

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  security.pam.services.sudo_local.touchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
