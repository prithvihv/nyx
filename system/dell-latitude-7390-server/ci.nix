{ config, pkgs, ... }:

{
  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.default = {
      enable = true;
      name = "dell-latitude-server-runner";
      url = "http://localhost:9753";
      # After deploying, get a token from http://192.168.0.51:9753/-/admin/runners
      # and write it to this file: sudo sh -c 'echo TOKEN > /var/lib/gitea-runner/token'
      tokenFile = "/var/lib/gitea-runner/token";
      labels = [ "latitude-7390-server:host" ];
    };
  };

  # Allow the runner to call nixos-rebuild with sudo (nothing else)
  security.sudo.extraRules = [
    {
      users = [ "gitea-runner" ];
      commands = [
        {
          command = "/run/current-system/sw/bin/nixos-rebuild";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
