#!/bin/sh
pushd ~/.nyx
git add .
nix build ~/.nyx\#darwinConfigurations.mw-pvirupa-GK4K.system

# https://xyno.space/post/nix-darwin-introduction
# printf 'run\tprivate/var/run\n' | sudo tee -a /etc/synthetic.conf # read below
# /System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -t # read below


/Users/prithvi.virupaksha/.nyx/result/sw/bin/darwin-rebuild switch --flake  ~/.nyx
popd

# darwin-rebuild switch -I darwin-config=$HOME/.nyx/darwin/configuration.nix
