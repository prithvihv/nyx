## Issues:
- [ ] move cron jobs to nix-store
- [ ] taffy bar has a memory leak: https://github.com/srid/nixos-config/issues/8 move to polybar
- [ ] tmux resurrect continuum not restoring automatically 

## Secret management
- using a sops to modules that accepts file params
- if other items need to be secured then we move to git-crypt

## Gamezop
- `vpn-action gzp-dev <start/stop>` connects and disconnects vpn, with DNS
- 