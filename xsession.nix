{ pkgs }:
let xmonadPhv = import ./xmonad-phv/default.nix;
in {
    xsession = {
        enable = true;
        windowManager.command = "${xmonadPhv}/bin/xmonad";
        # xmonad = {
        #     enable  = true;
        #     enableContribAndExtras = true;
        #     config  = xmonadPhv.xmonadHs; 
        # };
    };

    extraPkgs = with pkgs;[
        rofi
        clipmenu
        # picom
    ];
}
