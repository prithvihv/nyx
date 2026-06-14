{
  description = "configuration and workflows";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs"; # ask hm to use pinned nixpkgs

    # darwin stuff
    darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # extra
    elixir-extra.url = "github:hauleth/nix-elixir/master";
    elixir-extra.inputs.nixpkgs.follows = "nixpkgs";

    # authentik SSO / identity provider. Intentionally does NOT follow our
    # nixpkgs: authentik is version-sensitive and pins its own nixpkgs.
    authentik-nix.url = "github:nix-community/authentik-nix";

    # sbs
    # sbs.url = "git+ssh://git@github.com/wooga/sbs-nix.git";
    # sbs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      sops-nix,
      darwin,
      elixir-extra,
      nixos-unstable,
      authentik-nix,
      # sbs,
    }:
    let
      system = "x86_64-linux";
      insecurePakages = [
        "electron-27.3.11"
        # "yubikey-manager-qt-1.2.5"
      ];
      lib = nixpkgs.lib;
      linux-nixpkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          permittedInsecurePackages = insecurePakages;
        };
        overlays = [ elixir-extra.overlay ];
      };

      # Shared nixpkgs configuration for both Macs (mb-wooga + mbp-m4).
      darwinSystem = "aarch64-darwin";
      darwin-unstable-nixpkgs = import nixos-unstable {
        system = darwinSystem;
        config = {
          allowUnfree = true;
          permittedInsecurePackages = insecurePakages;
        };
      };

      darwin-nixpkgs = {
        config = {
          allowUnfree = true;
          allowUnsupportedSystem = true;
          permittedInsecurePackages = [
            "nodejs-14.21.3"
            "openssl-1.1.1u"
          ];
        };
        overlays = [
          (final: prev: { zola = darwin-unstable-nixpkgs.zola; })
          (final: prev: { openspec = darwin-unstable-nixpkgs.openspec; })
        ];
      };
    in
    {
      darwinConfigurations = {
        mw-pvirupa-GK4K = darwin.lib.darwinSystem {
          system = darwinSystem;
          modules = [
            ./darwin/mb-wooga/configuration.nix
            ./darwin/mb-wooga/homebrew.nix
            home-manager.darwinModules.home-manager
            {
              nixpkgs = darwin-nixpkgs;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users."prithvi.virupaksha" = import ./darwin/mb-wooga/home.nix;
              # home-manager.extraSpecialArgs = {
              #   sbs = sbs.packages.${darwinSystem};
              # };
            }
          ];
        };

        mbp-m4 = darwin.lib.darwinSystem {
          system = darwinSystem;
          modules = [
            ./darwin/mbp-m4/configuration.nix
            { system.configurationRevision = self.rev or self.dirtyRev or null; }
            # ./darwin/mbp-m4/homebrew.nix
            home-manager.darwinModules.home-manager
            {
              nixpkgs = darwin-nixpkgs;
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users."phv" = import ./darwin/mbp-m4/home.nix;
            }
          ];
        };
      };

      nixosConfigurations = {
        work-station = lib.nixosSystem {
          inherit system;
          pkgs = linux-nixpkgs;
          modules = [
            ./system/work-station/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.phv = import ./users/phv/home.nix;
            }
          ];
        };

        dell-latitude-7390 = lib.nixosSystem {
          inherit system;
          pkgs = linux-nixpkgs;
          modules = [
            ./system/dell-latitude-7390/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.phv = import ./users/phv/home.nix;
              home-manager.users.server = import ./users/server/home.nix;
            }
          ];
        };

        dell-latitude-7390-server = lib.nixosSystem {
          inherit system;
          pkgs = linux-nixpkgs;
          modules = [
            ./system/dell-latitude-7390-server/configuration.nix
            authentik-nix.nixosModules.default
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.server = import ./users/server/home.nix;
            }
          ];
        };
      };
    };
}
