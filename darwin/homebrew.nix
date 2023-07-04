{ config, pkgs, lib, ... }: {
  homebrew = {
    enable = true;
    onActivation.upgrade = true;
    # updates homebrew packages on activation,
    # can make darwin-rebuild much slower (otherwise i'd forget to do it ever though)
    casks = ["gpg-suite"] ++ [ "signal" "visual-studio-code" "amethyst" "chromium" "logseq" "datagrip" "spotify" "qtpass" "anki" "postman"];
  };
}
