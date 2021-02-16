{pkgs}:
with pkgs;
let 
  xmonadfile = pkgs.lib.readFile ./Main.hs;
in rec {
  xmonadHs = pkgs.writeText "xmonad.hs" xmonadfile;
#  alacritty = import ./alacritty/alacritty.nix { critty = pkgs.alacritty; inherit fish; }; # BEING USED RIGHT NOW
  programs = with pkgs;[
    rofi
    clipmenu
    # rofi-
  ];

  # alacritty is handled by home-nix.
}
