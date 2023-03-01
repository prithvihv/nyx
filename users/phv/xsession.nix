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
        "00ffffffffffff004d109a1400000000041c0104a52213780ede50a3544c99260f505400000001010101010101010101010101010101ac3780a070383e403020350058c210000018000000000000000000000000000000000000000000fe00544b365237804c513135364d31000000000002410328001200000a010a2020002b";
      DP-3 =
        "00ffffffffffff001e6d067719870700081f0103803c2278ea3e31ae5047ac270c50542108007140818081c0a9c0d1c081000101010108e80030f2705a80b0588a0058542100001e04740030f2705a80b0588a0058542100001a000000fd00383d1e873c000a202020202020000000fc004c472048445220344b0a202020013002033b714d9022201f1203040161605d5e5f230907076d030c002000b83c20006001020367d85dc401788003e30f0003e305c000e6060501605550023a801871382d40582c450058542100001e565e00a0a0a029503020350058542100001a000000ff003130384e54545145483333370a0000000000000000000000000000b2";
      DP-1 =
        "00ffffffffffff0010acdbd0424e5832201e010380351e78ea0565a756529c270f5054a54b00714f8180a9c0d1c00101010101010101023a801871382d40582c45000f282100001e000000ff00423736484635330a2020202020000000fc0044454c4c205032343139480a20000000fd00384c1e5311000a2020202020200118020317814c9010040302040401011f1f1365030c001000023a801871382d40582c45000f282100001e011d007251d01e206e2855000f282100001e011d007251d01e206e2855000f282100001e011d007251d01e206e2855000f282100001800000000000000000000000000000000000000000000000000000000000000007c";
    };

    laptopScreen = {
      # crtc = 0;
      primary = true;
      mode = "1920x1080";
      position = "0x0";
      rate = "60.00";
    };

    _4kScreen = {
      # crtc = 2;
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
          DP-3.enable = false;
          DP-1.enable = false;
        };
      };
      "focus_big" = {
        inherit fingerprint;
        config = {
          DP-3 = _4kScreen // {
            mode = "3840x2160";
            rotate = "normal";
          };
          eDP-1.enable = false;
          DP-1.enable = false;
        };
      };
      "focus_big_gaming" = {
        inherit fingerprint;
        config = {
          DP-3 = _4kScreen // {
            rotate = "normal";
            mode = "1920x1080";
          };
          eDP-1.enable = false;
          DP-1.enable = false;
        };
      };
      "triple_2k_horizontal_laptop" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen // {
            primary = false;
            position = "2240x1440";
          };
          DP-3 = _4kScreen // {
            position = "1920x0";
            rotate = "normal";
          };
          DP-1 = {
            # crtc = 1;
            enable = true;
            primary = false;
            mode = "1920x1080";
            position = "0x567";
            rotate = "normal";
          };
        };
      };
      "triple_2k_wide" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen // {
            primary = false;
            position = "4480x360";
          };
          DP-3 = _4kScreen // {
            rotate = "normal";
            position = "1920x0";
          };
          DP-1 = {
            # crtc = 1;
            enable = true;
            primary = false;
            mode = "1920x1080";
            position = "0x360";
            rotate = "normal";
          };
        };
      };
      "triple_2k_vertical_reader_left" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen // {
            primary = false;
            position = "3640x420";
          };
          DP-3 = _4kScreen // { position = "1080x180"; };
          DP-1 = {
            # crtc = 1;
            enable = true;
            primary = false;
            mode = "1920x1080";
            position = "0x0";
            rate = "60";
            rotate = "left";
          };
        };
      };
      "triple_1080p_vertical_reader_left" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen // {
            primary = false;
            position = "3640x420";
          };
          DP-3 = _4kScreen // {
            mode = "1920x1080";
            position = "1080x180";
          };
          DP-1 = {
            # crtc = 1;
            enable = true;
            primary = false;
            mode = "1920x1080";
            position = "0x0";
            rate = "60";
            rotate = "left";
          };
        };
      };

      "triple_4k_vertical_reader_left" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen // {
            primary = false;
            position = "3640x420";
          };
          DP-3 = _4kScreen // { position = "1080x180"; };
          DP-1 = {
            # crtc = 1;
            enable = true;
            primary = false;
            mode = "1920x1080";
            position = "0x0";
            rate = "60";
            rotate = "left";
          };
        };
      };
      "triple_2k_vertical_reader" = {
        inherit fingerprint;
        config = {
          eDP-1 = laptopScreen // { primary = false; };
          DP-3 = _4kScreen;
          DP-1 = {
            # crtc = 1;
            enable = true;
            primary = false;
            mode = "1920x1080";
            position = "4480x0";
            rate = "60";
            rotate = "right";
          };
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
