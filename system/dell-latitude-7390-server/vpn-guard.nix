# VPN confinement guard for the nixflix media stack.
#
# The whole point of nixflix.vpn is that qBittorrent (and optionally radarr /
# prowlarr) can ONLY reach the internet through the WireGuard tunnel — they run
# inside the `wg` network namespace, which has no route to the LAN gateway, so a
# tunnel drop is a blackhole, not a leak. The dangerous failure is the service
# silently NOT being confined at all (e.g. an upstream nixflix/VPN-Confinement
# refactor drops the wiring during an auto-update): it would then run on the
# normal network with full clearnet access, and nothing would complain.
#
# This module adds two independent guards against that:
#
#   Layer 1 (build time)  — assert each service that *should* be confined really
#                           has NetworkNamespacePath = /run/netns/wg. That is the
#                           actual kernel mechanism, so it can't be fooled by an
#                           option rename. If confinement disappears, the rebuild
#                           FAILS CLOSED instead of deploying a leaky config.
#
#   Layer 2 (runtime)     — a 30s systemd timer that checks each guarded
#                           service's live PID actually shares the wg netns
#                           (inode of /proc/<pid>/ns/net == inode of
#                           /run/netns/wg). On mismatch it stops + runtime-masks
#                           the service and pings Discord. Catches boot races and
#                           anything the build can't see.
#
# Discord webhook: written out-of-band (never in the Nix store) to
#   /var/lib/vpn-guard/secrets   ->   DISCORD_WEBHOOK_URL=https://discord.com/api/webhooks/...
# (root-owned, chmod 600). The EnvironmentFile is optional ('-'): without it the
# guard still stops/masks on a leak, it just can't send the notification.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  nsName = "wg";
  nsPath = "/run/netns/${nsName}";
  secretsFile = "/var/lib/vpn-guard/secrets";

  # Services to guard. Edit this list by hand to add radarr/prowlarr etc.
  guardedServices = [ "qbittorrent" ];

  # Post a message to a Discord webhook. Modeled on the Woodpecker notify-discord
  # script: fails loudly if DISCORD_WEBHOOK_URL is unset — a silently dropped
  # leak alert is worse than a noisy log line.
  notify-discord = pkgs.writeShellApplication {
    name = "vpn-guard-notify";
    runtimeInputs = with pkgs; [
      curl
      jq
      coreutils
    ];
    text = ''
      msg="''${1:?usage: vpn-guard-notify <message>}"

      if [ -z "''${DISCORD_WEBHOOK_URL:-}" ]; then
        echo "vpn-guard-notify: DISCORD_WEBHOOK_URL is not set" >&2
        exit 1
      fi

      payload="$(jq -nc --arg c "$msg" '{content: $c}')"

      curl -fsS -X POST \
        -H "Content-Type: application/json" \
        --data "$payload" \
        -w $'\nHTTP %{http_code}\n' \
        "$DISCORD_WEBHOOK_URL"
    '';
  };

  leak-check = pkgs.writeShellApplication {
    name = "vpn-guard-check";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.systemd
      notify-discord
    ];
    text = ''
      host="${config.networking.hostName}"
      nsPath="${nsPath}"

      enforce() {
        svc="$1"
        reason="$2"
        echo "vpn-guard: CONFINEMENT FAILURE for '$svc': $reason -- masking + stopping" >&2
        # Mask first (blocks any Restart= from racing us back up), --now stops it.
        systemctl mask --runtime --now "$svc" || true
        vpn-guard-notify "🚨 VPN GUARD [$host]: '$svc' was NOT inside the ${nsName} VPN namespace ($reason). It has been stopped and masked to prevent a clearnet leak. Investigate, then re-enable with: systemctl unmask $svc" || true
      }

      # Inode of the wg netns. If the namespace is gone, nothing should be
      # running that claims to be confined to it -> treat any running guarded
      # service as a failure.
      if [ -e "$nsPath" ]; then
        expected="$(stat -L -c %i "$nsPath")"
      else
        expected="__no_namespace__"
      fi

      for svc in ${lib.concatStringsSep " " guardedServices}; do
        pid="$(systemctl show -p MainPID --value "$svc" || true)"
        # Not running -> can't be leaking.
        if [ -z "$pid" ] || [ "$pid" = "0" ]; then
          continue
        fi

        if [ -r "/proc/$pid/ns/net" ]; then
          actual="$(stat -L -c %i "/proc/$pid/ns/net")"
        else
          actual="__unreadable__"
        fi

        if [ "$expected" = "__no_namespace__" ] || [ "$actual" != "$expected" ]; then
          enforce "$svc" "pid $pid is in netns '$actual', expected ${nsName} netns '$expected'"
        fi
      done
    '';
  };
in
{
  # Only active when something is actually meant to be confined.
  config = lib.mkIf (guardedServices != [ ]) {
    # ---- Layer 1: build-time guarantee (fail closed) ----------------------
    assertions = [
      {
        assertion = config.vpnNamespaces.${nsName}.enable or false;
        message = "vpn-guard: vpnNamespaces.${nsName} is not enabled, but service(s) ${lib.concatStringsSep ", " guardedServices} are expected to be VPN-confined.";
      }
    ]
    ++ map (svc: {
      assertion = (config.systemd.services.${svc}.serviceConfig.NetworkNamespacePath or null) == nsPath;
      message = "vpn-guard: systemd service '${svc}' is NOT confined to the ${nsName} VPN namespace (serviceConfig.NetworkNamespacePath != ${nsPath}). Refusing to build to prevent a clearnet leak. If you intentionally disabled its VPN, also remove it from nixflix's vpn.enable so it drops out of the guard.";
    }) guardedServices;

    # ---- Layer 2: runtime guarantee (defense in depth) --------------------
    systemd.tmpfiles.rules = [ "d /var/lib/vpn-guard 0700 root root -" ];

    systemd.services.vpn-guard = {
      description = "Verify VPN-confined services are inside the ${nsName} namespace";
      after = [ "${nsName}.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${leak-check}/bin/vpn-guard-check";
        EnvironmentFile = "-${secretsFile}";
      };
    };

    systemd.timers.vpn-guard = {
      description = "Periodic VPN confinement leak check";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "30s";
        AccuracySec = "5s";
      };
    };
  };
}
