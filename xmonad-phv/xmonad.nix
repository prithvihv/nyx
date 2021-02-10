{pkgs}:
with pkgs;
let 
  xmonadfile = pkgs.lib.readFile ./Main.hs;
in rec {
  xmonadHs = pkgs.writeText "xmonad.hs" xmonadfile;
#  alacritty = import ./alacritty/alacritty.nix { critty = pkgs.alacritty; inherit fish; }; # DONT BEING USED RIGHT NOW
  programs = [
#    alacritty
  ];

  # alacritty is handled by home-nix.
}
