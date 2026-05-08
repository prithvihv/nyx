{ config, pkgs, ... }:

{
  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.default = {
      enable = true;
      name = "dell-latitude-server-runner";
      url = "http://localhost:9753";
      tokenFile = "/var/lib/gitea-actions-runner-default/token";
      labels = [ "latitude-7390-server:host" ];
    };
  };

  # Allow the runner to call nixos-rebuild with sudo (nothing else)
  security.sudo.extraRules = [
    {
      users = [ "gitea-actions-runner-default" ];
      commands = [
        {
          command = "/run/current-system/sw/bin/nixos-rebuild";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
