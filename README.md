## Issues:
- [X] move cron jobs to nix-store
- [X] taffy bar has a memory leak: https://github.com/srid/nixos-config/issues/8 move to polybar
- [ ] tmux resurrect continuum not restoring automatically 
- [ ] learn and integrate tools to workflow:
    - [ ] ripgrep
    - [ ] fuzzy search
- [ ] GC for nix
- [ ] github p secrets
- [ ] i3 multi lock screen
- [ ] toggling notifications
- [ ] file manager broken
- [X] remove krusader and dependencies

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
