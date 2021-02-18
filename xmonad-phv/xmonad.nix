{ pkgs }:
with pkgs;
let 
  xmonadfile = pkgs.lib.readFile ./Main.hs;
in rec {
  xmonadHs = pkgs.writeText "xmonad.hs" xmonadfile;
#  alacritty = import ./alacritty/alacritty.nix { critty = pkgs.alacritty; inherit fish; }; # BEING USED RIGHT NOW
  programs = with pkgs;[
    rofi
    clipmenu

    # for taffy
    at-spi2-core # https://gist.github.com/jeffcogswell/62395900725acef1c0a5a608f7eb7a05
    hicolor-icon-theme
    # rofi-
  ];

  # alacritty is handled by home-nix.
}
