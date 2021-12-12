{
  description = "nyx config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    #  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs"; # ask hm to use pinned nixpkgs
     

    
  };

  outputs = { self, nixpkgs, home-manager }: 
  let
    system = "x86_64-linux"; 
    pkgs = import nixpkgs {
     inherit system;
     config = {
       allowUnfree = true;
     };
    };

    lib = nixpkgs.lib;
  in {
    nixosConfigurations = {
      nyx = lib.nixosSystem {
       inherit system;

       modules = [
         ./system/configuration.nix
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