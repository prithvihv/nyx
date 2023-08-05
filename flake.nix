{
  description = "configuration and workflows";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-21-11.url = "github:NixOS/nixpkgs/nixos-21.11-small";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    # neovim.url = "github:nix-community/neovim-nightly-overlay";
    # neovim.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows =
      "nixpkgs"; # ask hm to use pinned nixpkgs
  };

  outputs = { self, nixpkgs, nixpkgs-21-11, home-manager, sops-nix }:
    let
      system = "x86_64-linux";
      insecurePakages = [
        "electron-9.4.4"
        "electron-13.6.9"
        "electron-12.2.3"
        "electron-20.3.11"
      ];
      lib = nixpkgs.lib;
      # to upgrade discord: https://github.com/NixOS/nixpkgs/commit/1f2b951a1f68ae56986aa3831f0889615aa7ebaf
      overlay-discord = import ./pkgs/discord/discord.nix;
      overlay-signal = import ./pkgs/signal-desktop;
      overlay-gnupg = (self: super:
        with super; {
          gnupg = nixpkgs-21-11.legacyPackages.x86_64-linux.gnupg;
        });
    in {
      nixosConfigurations = {
        dell-latitude-7390 = let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              permittedInsecurePackages = insecurePakages;
            };
            overlays = [
              overlay-discord
              overlay-signal
              #neovim.overlay
            ];
          };
        in lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./system/dell-latitude-7390/configuration.nix
            # sops-nix.nixosModules.sops
            #  solana-nix.packages.x86_64-linux.cargo-build-bpf
            #  solana-nix.packages.x86_64-linux
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.phv = import ./users/phv/home.nix;
            }
          ];
        };

        nyx = let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              permittedInsecurePackages = insecurePakages;
            };
            overlays = [ overlay-discord overlay-gnupg overlay-signal ];
          };
        in lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./system/nyx/configuration.nix
            sops-nix.nixosModules.sops
            #  solana-nix.packages.x86_64-linux.cargo-build-bpf
            #  solana-nix.packages.x86_64-linux
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
