{ self, pkgs, ... }: {
  users.users."phv" = {
    name = "phv";
    home = "/Users/phv";
    shell = pkgs.fish;
    ignoreShellProgramCheck = true;
  };
  system.primaryUser = "phv";
  environment.shells = [ pkgs.fish ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [ pkgs.vim
    ];

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  security.pam.services.sudo_local.touchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
