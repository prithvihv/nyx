{
  description = "configuration and workflows";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows =
      "nixpkgs"; # ask hm to use pinned nixpkgs

    # darwin stuff
    darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # extra
    elixir-extra.url = "github:hauleth/nix-elixir/master";
    elixir-extra.inputs.nixpkgs.follows = "nixpkgs";

    # sbs
    sbs.url =
      "git+ssh://git@github.com/wooga/sbs-nix.git";
    # sbs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, sops-nix, darwin, elixir-extra
    , nixos-unstable, sbs }:
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
    in {
      darwinConfigurations.mw-pvirupa-GK4K = let
        system = "aarch64-darwin";
        darwin-unstable-nixpkgs = import nixos-unstable {
          inherit system;
          config = {
            allowUnfree = true;
            permittedInsecurePackages = insecurePakages;
          };
        };
        darwin-nixpkgs = {
          config = {
            allowUnfree = true;
            allowUnsupportedSystem = true;
            permittedInsecurePackages = [ "nodejs-14.21.3" "openssl-1.1.1u" ];
          };
          overlays = [
            (final: prev: { terraform = darwin-unstable-nixpkgs.terraform; })
          ];
        };
      in darwin.lib.darwinSystem {
        inherit system;
        modules = [
          ./darwin/mb-wooga/configuration.nix
          ./darwin/mb-wooga/homebrew.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs = darwin-nixpkgs;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users."prithvi.virupaksha" = import ./darwin/mb-wooga/home.nix;
            home-manager.extraSpecialArgs = { sbs = sbs.packages.${system}; };
          }
        ];
      };

      darwinConfigurations.mbp-m4 = let
        system = "aarch64-darwin";
        darwin-nixpkgs = {
          config = {
            allowUnfree = true;
            allowUnsupportedSystem = true;
            # permittedInsecurePackages = [ "nodejs-14.21.3" "openssl-1.1.1u" ];
          };
        };
      in darwin.lib.darwinSystem {
        inherit system;
        modules = [
          ./darwin/mbp-m4/configuration.nix
          {system.configurationRevision = self.rev or self.dirtyRev or null;}
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

      nixosConfigurations = {
        work-station = let pkgs = linux-nixpkgs;
        in lib.nixosSystem {
          inherit system;
          inherit pkgs;
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

        dell-latitude-7390 = let pkgs = linux-nixpkgs;
        in lib.nixosSystem {
          inherit system;
          inherit pkgs;
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
      };
    };
}
