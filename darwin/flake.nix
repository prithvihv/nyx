{
  description = "My first nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # nix will normally use the nixpkgs defined in home-managers inputs, we only want one copy of nixpkgs though
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs"; # ...

    neovim.url = "github:nix-community/neovim-nightly-overlay";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
  };

  # add the inputs declared above to the argument attribute set
  outputs = { self, nixpkgs, home-manager, darwin, neovim }: {
    darwinConfigurations.gzp-mbp = darwin.lib.darwinSystem {
      system = "aarch64-darwin"; # "x86_64-darwin" if you're using a pre M1 mac
      modules = [
        ./configuration.nix
        ./homebrew.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.phv = import ./home.nix;
        }
      ]; # will be important later
    };
  };
}
