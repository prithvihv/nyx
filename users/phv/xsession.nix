{ pkgs }:
let xmonadPhv = import ../../pkgs/xmonad-phv/default.nix { inherit pkgs; };
in {
    xsession = {
        enable = true;
        # windowManager.command = "sudo startx ${xmonadPhv}/bin/xmonad-x86_64-linux";
        windowManager.command = "${xmonadPhv}/bin/xmonad-x86_64-linux";
        # windowManager.command = "xmonad-x86_64-linux";

        # xmonad = {
        #     enable  = true;
        #     enableContribAndExtras = true;
        #     config  = xmonadPhv.xmonadHs; 
        # };
    };

    extraPkgs = with pkgs;[
        xmonadPhv
        rofi
        clipmenu
        xclip
        pinentry_qt
        feh
        lf
        arandr
        libnotify
        scrot
        dmenu
        # picom
    ];
}
