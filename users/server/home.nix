{ pkgs, config, lib, ... }: {
  enable = true;

  home.username = "server";
  home.homeDirectory = "/home/server";

  home.packages = with pkgs; [ git ];

  programs.htop = {
    enable = true;
    settings = { color_scheme = 6; };
  };
}
