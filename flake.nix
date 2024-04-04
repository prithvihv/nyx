{
  description = "configuration and workflows";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-21-11.url = "github:NixOS/nixpkgs/nixos-21.11-small";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    # neovim.url = "github:nix-community/neovim-nightly-overlay";
    # neovim.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows =
      "nixpkgs"; # ask hm to use pinned nixpkgs

    # darwin stuff
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # extra
    elixir-extra.url = "github:hauleth/nix-elixir/master";
    elixir-extra.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs = { self, nixpkgs, nixpkgs-21-11, home-manager, sops-nix, darwin
    , elixir-extra }:
    let
      system = "x86_64-linux";
      insecurePakages = [
        "electron-9.4.4"
        "electron-13.6.9"
        "electron-12.2.3"
        "electron-19.1.9"
        "electron-20.3.11"
        "electron-25.9.0"
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
      darwinConfigurations.mw-pprithv-GK4K = let
        system = "aarch64-darwin";
        nixpkgsConfig = {
          config = {
            allowUnfree = true;
            allowUnsupportedSystem = true;
            permittedInsecurePackages = [ "nodejs-14.21.3" "openssl-1.1.1u" ];
          };
        };
      in darwin.lib.darwinSystem {
        inherit system;
        modules = [
          ./darwin/configuration.nix
          ./darwin/homebrew.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs = nixpkgsConfig;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.phv = import ./darwin/home.nix;
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
            }
          ];
        };

        # TODO: deprecate nyx
        nyx = let
          overlay-gnupg = (self: super:
            with super; {
              gnupg = nixpkgs-21-11.legacyPackages.x86_64-linux.gnupg;
            });
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              permittedInsecurePackages = insecurePakages;
            };
            overlays = [ overlay-gnupg ];
          };
        in lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./system/nyx/configuration.nix
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.phv = import ./users/phv/home.nix;
            }
          ];
        };
      };
    };
}
