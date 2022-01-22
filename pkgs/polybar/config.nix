{ pkgs }:
let
  launchScriptStr=  ''
    # Terminate already running bar instances
      # ${pkgs.psmisc}/bin/killall -q polybar
      ${pkgs.polybar}/bin/polybar-msg cmd quit
    # Wait until the processes have been shut down
      while ${pkgs.procps}/bin/pgrep -u $UID -x polybar >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; done
      
      export WLIF=$(${pkgs.coreutils}/bin/tail -n +3 /proc/net/wireless | ${pkgs.gawk}/bin/gawk -F':' '{print $1}')
      echo $WLIF
      for m in $(${pkgs.xorg.xrandr}/bin/xrandr --query | ${pkgs.gnugrep}/bin/grep " connected" | ${pkgs.coreutils}/bin/cut -d" " -f1); do
        #MONITOR=$m polybar -l info --reload top &
        MONITOR=$m polybar top & disown
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
in {
  extraPkgs = [ launchPolybar pkgs.xmonad-log ];

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
      "bar/top" = {
        bottom = false;
        fixed-center = true;
        enable-ipc = true;
        monitor = "\${env:MONITOR:}";

        height = 19;
        # height = "2%";
        width = "100%";
        # radius = 0;

        locale = "en_US.UTF-8";
        modules-right = "volume date";
        modules-center = "time";
        modules-left = "xmonad";
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
    };

  };
}
