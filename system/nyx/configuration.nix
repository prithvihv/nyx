# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

inputs@{ lib, config, pkgs, ... }:

let
  gzp-vpn = import ./../../priv/gzp-stuff.nix { inherit lib; };
  hroneTokenScript = import ./../../priv/hrone.token.nix { inherit pkgs; };
in {
  imports = [ # Include the results of the hardware scan.
    ../common.nix
    ./hardware-configuration.nix
  ];

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraConfig = ''
    phv ALL=(ALL) NOPASSWD: ALL
  '';

  services.xserver.videoDrivers = [ "nvidia" ];

  nixpkgs.config.packageOverrides = pkgs: rec {
    vscodeCpp = pkgs.vscode-with-extensions.override {
      vscodeExtensions = vscode-extensions:
        with vscode-extensions;
        [ ms-vscode.cpptools ];
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nyx"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.extraHosts = ''
    127.0.0.1 bake.sale
  '';

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp59s0.useDHCP = true;
  networking.wg-quick.interfaces = {
    gzp-dev = gzp-vpn.gzp-dev;
    gzp-prd = gzp-vpn.gzp-prd;
  };

  # Secret management
  sops = {
    defaultSopsFile = ../../sops/secrets/nyx.yaml;
    age.keyFile = "/home/phv/.keybox/age.sops.txt";
    secrets = let
      secrets = [
        "vpn/gzp_dev/private"
        "vpn/gzp_prd/private"
        "secret_name" # need some defaults, for nested paths to work
      ];
      permissionSettings = {
        mode = "0440";
        owner = config.users.users.phv.name;
        group = config.users.users.phv.group;
      };
      defaultPermissions = secret: { ${secret} = permissionSettings; };
    in lib.foldl' lib.mergeAttrs { } (builtins.map defaultPermissions
      secrets); # building map with keys of secret paths
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # services.dbus.packages = [ pkgs.gcr ];

  # Enable the X11 windowing system.

  hardware.opengl = {
    enable = true;
    extraPackages = [
      pkgs.vaapiIntel
      pkgs.vaapiVdpau
      pkgs.libvdpau-va-gl
      pkgs.intel-media-driver
    ];
  };

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
      # sync.enable = true; keeps GPU on permenently
      offload.enable = true;
    };
    modesetting.enable = true;
  };

  services.cron = {
    enable = true;
    # FIXME: add shellscripts to nixstore
    systemCronJobs = [
      "0 10,11,20,22 * * 1-5      phv    /home/phv/.nyx/users/phv/cron/hrone.sh"
      "0 9 * * *        phv  ${hroneTokenScript}/bin/hrone_token"
      # "*/10 * * * *  phv  /home/phv/.nyx/users/phv/cron/killtaffy.sh"
    ];
  };
  services.prometheus = {
    enable = false;
    retentionTime = "13d";
    exporters = {
      node = {
        # TODO: takes CPU FOR no reason
        enable = false;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    scrapeConfigs = [
      # {
      #   job_name = "nyx";
      #   static_configs = [{ targets = [ "127.0.0.1:9002" ]; }];
      #   scrape_interval = "15s";
      # }
      {
        job_name = "local-bidbook";
        static_configs = [{ targets = [ "localhost:4040" ]; }];

        # nixos Master has authorization
        bearer_token = "1d5d6789538cf812ee128f357b610232";
        scrape_interval = "1s";
      }
    ];
  };

  services.grafana = {
    enable = true;
    settings = {
      "analytics.reporting".enable = false;
      "auth.anonymous".enable = false;

      database = {
        name = "grafana";
        type = "postgres";
        host = "localhost";
        user = "postgres";
        password = "postgres";
      };
      extraOptions = { PANELS_DISABLE_SANITIZE_HTML = "true"; };
    };

    declarativePlugins = with pkgs.grafanaPlugins; [ grafana-piechart-panel ];
  };

  services.jenkins = {
    enable = false;
    jobBuilder = {
      enable = true;
      nixJobs = [{
        job = {
          name = "jenkins-job-test-3";
          builders = [{ shell = "echo 'Hello world!'"; }];
        };
      }];
    };
  };

  # services.yubikey-agent = { enable = true; };
  # services.pcscd.enable = true;
  # services.udev.packages = with pkgs; [ yubikey-personalization ];

  services.zookeeper = { enable = false; };
  services.apache-kafka = { enable = false; };
  services.k3s = {
    enable = false;
    role = "server";
  };

  programs.ssh = { startAgent = true; };
  programs.steam = {
    enable = true;
    # remotePlay.openFirewall =
    #   true; # Open ports in the firewall for Steam Remote Play
    # dedicatedServer.openFirewall =
    #   true; # Open ports in the firewall for Source Dedicated Server
  };

  environment.variables = {
    EDITOR = "nvim";
    NEOVIDE_MULTIGRID = "true";
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  # trying to get printer working
  hardware.sane = {
    enable = false;
    extraBackends = [ pkgs.hplipWithPlugin ];
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    let
      nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
        export __NV_PRIME_RENDER_OFFLOAD=1
        export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
        export __GLX_VENDOR_LIBRARY_NAME=nvidia
        export __VK_LAYER_NV_optimus=NVIDIA_only
        exec "$@"
      '';
    in [
      #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      wget
      vim
      hello

      nvidia-offload
    ];
  virtualisation.docker.enable = true;
  virtualisation.lxd.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg = {
  #   package = pkgs.gnupg1orig;
  #   agent = {
  #     enable = true;
  #     # enableExtraSocket = true;
  #     pinentryFlavor = "qt";
  #     # enableSSHSupport = true;
  #   };
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}

