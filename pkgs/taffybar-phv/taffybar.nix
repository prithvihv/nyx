{ pkgs }:
let
  taffyPkg = import ./default.nix { inherit pkgs; };
in {
    enable = true;
    package = taffyPkg;
}