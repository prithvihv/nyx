{ pkgs }:
    let xmonadPhv = import ./xmonad-phv/xmonad.nix { inherit pkgs;};
in {
    xsession = {
        enable = true;
        windowManager.xmonad = {
            enable  = true;
            enableContribAndExtras = true;
            config  = xmonadPhv.xmonadHs; 
        };
    };

    extraPkgs = [ ] ++ xmonadPhv.programs;
}
