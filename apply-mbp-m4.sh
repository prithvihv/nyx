#!/bin/sh
pushd ~/.nyx
git add .

sudo darwin-rebuild switch --flake  ~/.nyx#mbp-m4

popd

# darwin-rebuild switch -I darwin-config=$HOME/.nyx/darwin/configuration.nix
# sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin/nix-darwin-25.05#darwin-rebuild -- switch --flake ~/.nyx#mbp-m4
