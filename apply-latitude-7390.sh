#!/bin/sh
pushd ~/.nyx
git add .
# sudo nixos-rebuild switch -I nixos-config=./system/configuration.nix
sudo nixos-rebuild switch --flake .#dell-latitude-7390
popd
