{
  description = "nyx config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixpkgs-21-11.url = "github:NixOS/nixpkgs/nixos-21.11";
    # nixpkgs.url = "github:NixOS/nixpkgs/master";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-22.05";
    # solana-nix.url = "github:drozdziak1/nix-solana-sdk/master";
    home-manager.inputs.nixpkgs.follows =
      "nixpkgs"; # ask hm to use pinned nixpkgs
  };

  outputs = { self, nixpkgs, nixpkgs-21-11, home-manager, sops-nix }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          permittedInsecurePackages = [ "electron-9.4.4" "electron-13.6.9" ];
        };
        overlays = [
          (self: super:
            with super; {
              gnupg = nixpkgs-21-11.legacyPackages.x86_64-linux.gnupg;
            })
        ];
      };

      lib = nixpkgs.lib;
    in {
      nixosConfigurations = {
        nyx = lib.nixosSystem {
          inherit system;
          inherit pkgs;
          modules = [
            ./system/configuration.nix
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
