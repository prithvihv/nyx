{ pkgs, lib, ... }:
let
  fishConfig = import ../../pkgs/fish.nix {
    inherit pkgs lib;
    isWoogaMachine = false;
  };
  gitConfig = import ../../pkgs/git.nix { inherit pkgs; };
  alacrittyConfig = import ../../pkgs/alacritty.nix { };
in
{
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish = fishConfig;
  programs.git = gitConfig;
  programs.alacritty = alacrittyConfig;

  # enable puts emacs + emacsclient on PATH; the daemon (services.emacs) uses
  # this same package. emacs30-macport is the native macOS GUI build.
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-macport;
  };

  services.emacs = {
    enable = true;
    client = { enable = true; };
  };

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    qtpass
    pass
    gnutar
    gnupg # does not show up in Application dir, currently openning it manually
    nixd # nix language server
    nil # nix language server
    nixfmt
    openspec # spec-driven dev CLI; bundles node privately, no global nodejs
    ripgrep
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # The state version is required and should stay at the version you
  # originally installed.
  home.stateVersion = "25.05";
}
