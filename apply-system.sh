#!/bin/sh
pushd ~/.nyx
# sudo nixos-rebuild switch -I nixos-config=./system/configuration.nix
sudo nixos-rebuild switch --flake .#
popd
