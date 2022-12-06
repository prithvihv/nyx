{ pkgs }:

# wallpaper credits: https://github.com/gytis-ivaskevicius/high-quality-nix-content/blob/master/wallpapers/nix-glow-black.png
let
  # files
  nixWallpaperFile = ../wallpapers/nix-glow-black.png;

  # combo functions
  simpleWallpaper = file-path:
    pkgs.writeText "onAttachMonitor" ''
      ${pkgs.feh}/bin/feh --bg-scale ${file-path}
    '';

  # sets
  nixWallepaper = simpleWallpaper nixWallpaperFile;
in nixWallepaper
