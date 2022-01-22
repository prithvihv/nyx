{ pkgs }:
let
  getPrimaryMonitorScriptStr = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --query | ${pkgs.gnugrep}/bin/grep " connected" | grep "primary" | ${pkgs.coreutils}/bin/cut -d" " -f1 | head -n 1
  '';

  getPrimaryMonitorScript = pkgs.writeShellScriptBin "getPrimaryMonitorScript"
    getPrimaryMonitorScriptStr;

  launchScriptStr = ''
    # Terminate already running bar instances
      # ${pkgs.psmisc}/bin/killall -q polybar
      ${pkgs.polybar}/bin/polybar-msg cmd quit
    # Wait until the processes have been shut down
      while ${pkgs.procps}/bin/pgrep -u $UID -x polybar >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; done
      
      export WLIF=$(${pkgs.coreutils}/bin/tail -n +3 /proc/net/wireless | ${pkgs.gawk}/bin/gawk -F':' '{print $1}')
      echo $WLIF

      export PMONITOR=$(${getPrimaryMonitorScript}/bin/getPrimaryMonitorScript)

      echo $PMONITOR

      for m in $(${pkgs.xorg.xrandr}/bin/xrandr --query | ${pkgs.gnugrep}/bin/grep " connected" | ${pkgs.coreutils}/bin/cut -d" " -f1); do

        #MONITOR=$m polybar -l info --reload top &

        # place tray on primary monitor
        if [[ $PMONITOR = $m ]]
        then
          export POLY_TRAY=right
        else
          export POLY_TRAY=none
        fi

        MONITOR=$m WLIF=$WLIF POLY_TRAY=$POLY_TRAY polybar top & disown
      done
  '';

  launchPolybar = pkgs.writeShellScriptBin "launchPolybar" launchScriptStr;

  common = {
    cursor-click = "pointer";
    font-0 = "Iosevka:size=12;3";
    font-1 = "Noto Color Emoji:style=Regular:scale=10;2";
    font-2 = "Font Awesome 5 Free Regular:style=Regular:size=12;0";
    font-3 = "FuraCode Nerd Font:style=Bold:size=12;3";
  };

  primary = "#504945";

  secondary = "#ebdbb2";

  tertiary = "#1d2021";

  quaternary = "#ecf0f1";
  urgency = "#e74c3c";
in {
  extraPkgs = [ launchPolybar pkgs.xmonad-log ];
  launchPolybar = launchPolybar;

  home-manager-config = {
    enable = true;
    package = pkgs.polybarFull.override {
      # i3GapsSupport = true;
      alsaSupport = true;
      # iwSupport = true;
      # githubSupport = true;
      pulseSupport = true;
    };
    #   script = "polybar top";
    script = launchScriptStr;
    settings = {

      # nesting is not allowed in the "config" block
      "module/volume" = {
        type = "internal/pulseaudio";
        format.volume = "<ramp-volume> <label-volume>";
        label.muted.text = "🔇";
        label.muted.foreground = "#666";
        ramp.volume = [ "🔈" "🔉" "🔊" ];

        click.right = "pavucontrol &";
      } // common;
    };

    # https://github.com/polybar/polybar/wiki/Configuration
    config = {
      "settings" = {
        throttle-output = 5;
        throttle-output-for = 10;

        screenchange-reload = true;

        compositing-background = "source";
        compositing-foreground = "over";
        compositing-overline = "over";
        comppositing-underline = "over";
        compositing-border = "over";

        pseudo-transparency = "false";

        # format-foreground = 10;
        # format-background = 10;
        # format-underline = 10;
        # format-overline = 10;
        # format-spacing = 10;
        # format-padding = 10;
        # format-margin = 1;
        # format-offset = 10;
      };

      "bar/top" = {
        bottom = false;
        fixed-center = true;
        enable-ipc = true;
        monitor = "\${env:MONITOR:}";
        module-margin = 1;

        height = 19;
        # height = "2%";
        width = "100%";
        # radius = 0;

        locale = "en_US.UTF-8";
        modules-right = "wireless volume battery date";
        modules-center = "time";
        modules-left = "xmonad";
        # tray-position = "\${env:MONITOR:}";
        tray-position = "\${env:POLY_TRAY:}";
      } // common;

      "global/wm" = {
        margin-bottom = 0;
        margin-top = 0;
      };

      "module/xmonad" = {
        type = "custom/script";
        exec = "${pkgs.xmonad-log}/bin/xmonad-log";
        tail = true;
      };

      "module/time" = {
        type = "internal/date";
        time = "%H:%M:%S";
        label = "%time%";
      };

      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%d.%m.%y";

        label = "%date%";
      };

      "module/battery" = {
        type = "internal/battery";
        # full-at = 99;
        battery = "BAT0";
        adapter = "AC";

        poll-interval = 5;

        label-full = " 100%";
        format-full-padding = 1;
        format-full-foreground = secondary;
        format-full-background = primary;

        format-charging = " <animation-charging> <label-charging>";
        format-charging-padding = 1;
        format-charging-foreground = secondary;
        format-charging-background = primary;
        label-charging = "%percentage%% %time%";
        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";
        animation-charging-framerate = 500;

        format-discharging = "<ramp-capacity> <label-discharging>";
        format-discharging-padding = 1;
        format-discharging-foreground = secondary;
        format-discharging-background = primary;
        label-discharging = "%percentage%% %time%";
        ramp-capacity-0 = "";
        ramp-capacity-0-foreground = urgency;
        ramp-capacity-1 = "";
        ramp-capacity-1-foreground = urgency;
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
      };
      "module/wireless" = {
        type = "internal/network";
        # interface = "wlp59s0";
        interface = "\${env:WLIF:}";
        interval = "3.0";
        format-connected-padding = 1;
        format-connected-foreground = secondary;
        format-connected-background = primary;
        format-connected = " <label-connected>";
        label-connected = "%essid%";
      };
    };

  };
}
