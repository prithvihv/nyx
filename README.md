## Issues:
- [X] move cron jobs to nix-store
- [X] taffy bar has a memory leak: https://github.com/srid/nixos-config/issues/8 move to polybar
- [ ] tmux resurrect continuum not restoring automatically 
- [ ] learn and integrate tools to workflow:
    - [ ] ripgrep
    - [ ] fuzzy search
- [ ] GC for nix
- [ ] github p secrets
- [X] i3 multi lock screen
- [ ] toggling notifications
- [ ] file manager broken
- [X] remove krusader and dependencies
- [ ] switch to i3 blocks
- [ ] switch to [vitesse](https://marketplace.visualstudio.com/items?itemName=antfu.theme-vitesse&ssr=false#overview) color palette

## MacOS TODO:
- [ ] commit nixos
- [ ] setup SSH
- [ ] sign in Gamezop 

## Docs management linux
- use convert to convert images into pdfs
- pandoc to convert from other formats to pdfs like markdown

# XPS hardware config references:
https://gist.github.com/matthewbauer/68775d50d371eafb0de41a49f81f9cca

## Secret management
- using a sops to modules that accepts file params
- if other items need to be secured then we move to git-crypt

## Sops adding secrets
- run `make editSopsSecret` with secrets in env
- need to add key in configuration.nix

## Gamezop
- `vpn-action gzp-dev <start/stop>` connects and disconnects vpn, with DNS

## Scanner
https://nixos.wiki/wiki/Scanners


## Random
- https://www.reddit.com/r/programming/comments/q0oqai/what_is_wrong_with_geeksforgeeks_why_forcing_to/ <- add this to ublock origin
- remember to `git add .`  before `nixos-rebuild`
- To get `command-not-found` working, need to a channel and run update. https://discourse.nixos.org/t/command-not-found-unable-to-open-database/3807/7

# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Added for new features.
### Changed for changes in existing functionality.
### Deprecated for soon-to-be removed features.
### Removed for now removed features.
### Fixed for any bug fixes.
### Security in case of vulnerabilities.


## [phv3] - 2022-08-13
### Added
- nvidia drivers / steam
## [phv2] - 2022-01-23
### Added for new features.
- autorandr setup

## [phv1] - 2022-01-22
### Added for new features.
- polyBar 
- ALOT of fonts
- xmonad log push on bus
### Changed
- dracula for rofi

## [phv] - unknown


## TODO: 
- deprecate SOPs for git crypt
    moved `~/.config/sops/age/keys.txt` need to clean up file on ~/.keybox
- record all VSCODE plugins properly


- fzf: https://andrew-quinn.me/fzf/


### LUKS:
- https://scs.community/2023/02/24/impact-of-disk-encryption/
- overview: https://infosecwriteups.com/how-luks-works-with-full-disk-encryption-in-linux-6452ad1a42e8
- broke pendrive: https://ubuntuforums.org/showthread.php?t=2472411
    `cryptsetup -y -v luksFormat /dev/sdb1` failed then pendrive was write protected

## wipe
- fastest way to wipe whole disk might be to use in build feature: https://unix.stackexchange.com/questions/553143/how-can-i-speed-up-secure-erasing-of-a-disk, might now be applicable to your harddisk
- https://superuser.com/questions/831486/complete-wiping-of-hard-drive-shred-wipe-or-dd
- https://manpages.ubuntu.com/manpages/impish/en/man1/srm.1.html

### YUBIKEY:
https://github.com/drduh/YubiKey-Guide#nixos


### Minor things to keep in mind when buying next used laptop:
1. home and end keys and their locations, they are located at the top for some keyboard and are not remapable to fn + arrow keys. You can use `xev` to test this.

udo tar -czf ./backup2.tar.gz --exclude=./backup2.tar.gz --exclude=./backup.tar.
gz --exclude=/dev --exclude=/mnt --exclude=/proc --exclude=/sys --exclude=/tmp --exclude=/lost+found --exclude=/nix --exclude=/
var/lib/docker --exclude=/var/lib/postgresql --exclude=/swapfile --exclude=/nix --exclude=/home/phv/.cache --exclude=/hom
e/phv/.stack --exclude=/home/phv/.cabal --exclude=/home/phv/.minikube --exclude=/home/phv/.npm --exclud
e=/home/phv/.pnpm-store --exclude=/home/phv/.mozilla --exclude=/home/phv/.ivy2 --exclude=/home/phv/.sbt
 --exclude=/home/phv/.rustup --exclude=/home/phv/.nvm /

rm -rf /home/phv/.cabal /home/phv/.minikube /home/phv/.npm /home/phv/.pnpm-store /home/phv/.ivy2 /home/phv/.sbt /home/phv/.rustup /home/phv/go /home/phv/code

## darwin TODO:

configure automator something like: https://github.com/MatthewWest/pass-alfred#readme

## General notes on darwin:
-  need to install homebrew, gpg suite manually
