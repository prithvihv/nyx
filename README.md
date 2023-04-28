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

## Random
https://www.reddit.com/r/programming/comments/q0oqai/what_is_wrong_with_geeksforgeeks_why_forcing_to/ <- add this to ublock origin

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