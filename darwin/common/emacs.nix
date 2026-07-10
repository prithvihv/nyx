{ config, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-macport;
  };

  # Plain terminal Emacs as the default editor (e.g. git commit messages).
  # No daemon/server: each call starts its own short-lived Emacs. home-manager
  # exposes this to fish/zsh/bash through its generated session-variables files.
  home.sessionVariables.EDITOR = "emacs -nw";

  # Nix manages the Emacs *binary*; the Emacs Lisp config lives in the repo as
  # a live-editable file. This symlinks ~/.config/emacs/init.el straight to the
  # working copy (an "out of store" symlink), so editing init.el takes effect
  # immediately WITHOUT a darwin-rebuild. Packages install themselves at runtime
  # (package.el + MELPA), so adding a package is also rebuild-free.
  #
  # Assumes this repo is checked out at ~/.nyx (true on both Macs).
  xdg.configFile."emacs/init.el".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.nyx/darwin/common/emacs/init.el";
}
