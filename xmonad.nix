{pkgs}:
with pkgs;
let 
  xmonadfile = pkgs.lib.readFile ./xmonad-phv/Main.hs;
in rec {
  xmonad_ = pkgs.writeText "xmonad.hs" xmonadfile;
#  alacritty = pkgs.alacritty;
  programs = [
    alacritty
  ];
}
