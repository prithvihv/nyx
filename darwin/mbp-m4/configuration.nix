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

  sops = {
    defaultSopsFile = ../../secrets/common.yaml;
    # Decrypt with phv's ssh key, which is the `mbp_m4` recipient in .sops.yaml.
    # The default here would be /etc/ssh/ssh_host_ed25519_key, 
    age.sshKeyPaths = [ "/Users/phv/.ssh/id_ed25519" ];
    # Defaults to /etc/ssh/ssh_host_rsa_key, which sops would try to import as a
    # GPG key. Nothing here is encrypted to GPG.
    gnupg.sshKeyPaths = [ ];
    secrets.placeholder = { };
  };

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  security.pam.services.sudo_local.touchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
