# Since the xmonad config will be built by nixos-rebuild, we use the
# nix-channel's nixpkgs.
{ pkgs ? import <nixpkgs> {} }:
let 
in 
  pkgs.haskellPackages.developPackage {
    name = "taffybar-phv";
    root = ./.;
    modifier = drv:
     pkgs.haskell.lib.addBuildTools drv
      (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
  }
