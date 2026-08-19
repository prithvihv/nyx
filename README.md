# nyx

NixOS and nix-darwin flake configuring all my machines.

## Hosts

| Host | Flake target | Apply with |
| --- | --- | --- |
| Home server (Dell Latitude 7390) | `dell-latitude-7390-server` | `./apply-latitude-7390-server.sh` |
| Laptop (Dell Latitude 7390) | `dell-latitude-7390` | `./apply-latitude-7390.sh` |
| Workstation (LUKS-encrypted) | `work-station` | `./apply-work-station.sh` |
| Work MacBook | `mw-pvirupa-GK4K` | `./apply-mb-wooga.sh` |
| Personal MacBook (M4) | `mbp-m4` | `./apply-mbp-m4.sh` |

Each script is a thin wrapper around `nixos-rebuild switch --flake .#<target>`
(`darwin-rebuild` on macOS). They run `git add .` first, since flakes ignore
untracked files.

The server also rebuilds itself: `.woodpecker.yml` runs `nixos-rebuild switch` on
every push to `main` or `server-*`.

## Layout

| Path | Contents |
| --- | --- |
| `system/` | NixOS host configs; `common.nix` is shared by the laptop and workstation |
| `darwin/` | macOS host configs |
| `users/` | home-manager configs |
| `pkgs/` | per-program modules and custom packages |
| `sops/` | encrypted secrets |

## Server services

Everything is fronted by Caddy on 80/443 and published under two domain trees:
`local.prithvihv.xyz` (LAN, resolved by Pi-hole) and `tailscale.prithvihv.xyz`
(tailnet). Backends bind to loopback. 
`system/dell-latitude-7390-server/ingress.nix` is an interesting file. 