{ pkgs, lib, launchPolybar }:
let
  xmonadPhv = import ../../pkgs/xmonad-phv/default.nix { inherit pkgs; };
  wallpaperStr = lib.readFile (pkgs.callPackage ./scripts/wallpaper.nix { });

  launchPolybarStr = lib.readFile "${launchPolybar}/bin/launchPolybar";

  rofiAutorandr = pkgs.writeShellScriptBin "rofiAutorandr" ''
    layout=$(${pkgs.autorandr}/bin/autorandr | rofi -dmenu -p "Layout")
    ${pkgs.autorandr}/bin/autorandr --load $layout
  '';
  # TODO: on switching need to change DPI
  # https://blog.summercat.com/configuring-mixed-dpi-monitors-with-xrandr.html
  # Graphics card is configured can consider using 4k now
in {
  # to fill this part
  # use autorandr --config
  # and autorandr --fingerprint
  autorandr = let
    fingerprint = {
      eDP-1 =
        "00ffffffffffff0030e4480500000000001a0104a51d1178ea0c35a251469d270c505400000001010101010101010101010101010101293680a070381f403020350026a51000001a1e2480a070381f403020350026a51000001a000000fe004c4720446973706c61790a2020000000fe004c503133335746352d535044320056";
      HDMI-1 =
        "00ffffffffffff001e6d067719870700081f0103803c2278ea3e31ae5047ac270c50542108007140818081c0a9c0d1c081000101010108e80030f2705a80b0588a0058542100001e04740030f2705a80b0588a0058542100001a000000fd00383d1e873c000a202020202020000000fc004c472048445220344b0a202020013002033b714d9022201f1203040161605d5e5f230907076d030c001000b83c20006001020367d85dc401788003e30f0003e305c000e6060501605550023a801871382d40582c450058542100001e565e00a0a0a029503020350058542100001a000000ff003130384e54545145483333370a0000000000000000000000000000c2";
    };

    laptopScreen = {
      # crtc = 0;
      primary = true;
      mode = "1920x1080";
      position = "0x0";
      rate = "60.00";
    };

    _4kScreen = {
      crtc = 1;
      primary = true;
      mode = "2560x1440";
      position = "1920x0";
      rate = "59.95";
    };
  in {
    enable = true;
    hooks = {
      postswitch = {

        # TODO: to support 4k screen properly after switching check if it's on and run scale
        "change-background" = wallpaperStr;
        "launchPolybar" = launchPolybarStr;
      };
    };
    # get these by saving a arandr config to a a file, 
    # reading the file to get arguments
    profiles = {
      "portable" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen;
          # DP-3.enable = false;
          # DP-1.enable = false;
        };
      };
      "focus_big" = {
        inherit fingerprint;
        config = {
          HDMI-1 = _4kScreen // {
            mode = "3840x2160";
            rotate = "normal";
          };
          eDP-1.enable = false;
        };
      };
      "focus_big_gaming" = {
        inherit fingerprint;
        config = {
          HDMI-1 = _4kScreen // {
            rotate = "normal";
            mode = "1920x1080";
          };
          eDP-1.enable = false;
        };
      };
    };
  };

  xsession = {
    enable = true;
    initExtra = ''
      ${wallpaperStr}
      ${pkgs.volctl}/bin/volctl & disown
      ${launchPolybarStr}
      export XMODIFIERS="@im=fcitx5"
      export XMODIFIER="@im=fcitx5"
      export GTK_IM_MODULE="fcitx5"
      export QT_IM_MODULE="fcitx5"
      fcitx5 &

      # FIXME: this should be golang config
      go env -w GOPRIVATE=github.com/gamezop
    '';
    windowManager.command = "${xmonadPhv}/bin/xmonad-x86_64-linux";

    # xmonad = {
    #     enable  = true;
    #     enableContribAndExtras = true;
    #     config  = xmonadPhv.xmonadHs; 
    # };
  };

  extraPkgs = with pkgs; [
    xmonadPhv
    rofi
    clipmenu
    xclip
    pinentry_qt
    feh
    lf
    libnotify
    scrot
    dmenu
    i3lock

    # a/v
    brightnessctl
    pavucontrol
    volctl
    arandr
    betterlockscreen

    rofiAutorandr
  ];
}
