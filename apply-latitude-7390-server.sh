#!/bin/sh
DIR="$(cd "$(dirname "$0")" && pwd)"
pushd "$DIR"
git add .
# sudo nixos-rebuild switch -I nixos-config=./system/configuration.nix
sudo nixos-rebuild switch --flake .#dell-latitude-7390-server
popd
