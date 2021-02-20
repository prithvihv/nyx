{ pkgs }:
{
    enable = true;
    package =
      polybar.override {
        alsaSupport = true;
        i3GapsSupport = true;
        iwSupport = true;
        nlSupport = false;
      };
    config = ./config/polybar/polybarrc;
    script = builtins.readFile "${polybarLaunch}/bin/launchPolybar";
}