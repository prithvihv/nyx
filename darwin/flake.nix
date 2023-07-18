{
  description = "My first nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # nix will normally use the nixpkgs defined in home-managers inputs, we only want one copy of nixpkgs though
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    neovim.url = "github:nix-community/neovim-nightly-overlay";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
  };

  # add the inputs declared above to the argument attribute set
  outputs = { self, nixpkgs, home-manager, darwin, neovim }: let 
     system = "aarch64-darwin"; # "x86_64-darwin" if you're using a pre M1 mac
 nixpkgsConfig = {
  config = {
    allowUnfree = true;
    allowUnsupportedSystem = true;
    permittedInsecurePackages = [ "nodejs-14.21.3"   "openssl-1.1.1u"];
  };};
   in {
    darwinConfigurations.mw-pprithv-GK4K = darwin.lib.darwinSystem {
     inherit system;
      modules = [
        ./configuration.nix
        ./homebrew.nix
        home-manager.darwinModules.home-manager
        { 
nixpkgs = nixpkgsConfig;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.phv = import ./home.nix;
        }
      ]; # will be important later
    };
  };
}
