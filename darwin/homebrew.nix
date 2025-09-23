{ config, pkgs, lib, ... }: {
  homebrew = {
    enable = true;
    onActivation.upgrade = true;
    brews = [ "saml2aws" ] ++ [
      # needs these for erlang asdf
      # https://github.com/asdf-vm/asdf-erlang#osx
      "ncurses"
      "autoconf"
      "wxwidgets"
      "libxslt"
      "fop"
    ];
    # updates homebrew packages on activation,
    # can make darwin-rebuild much slower (otherwise i'd forget to do it ever though)
    casks = [ "gpg-suite" "yubico-yubikey-manager" ] ++ [
      "vlc"
      "signal"
      "spotify"
      # "alfred"
    ] ++ [
      # productivity
      "maccy"
      "amethyst"
      "logseq"
    ] ++ [
      # devtools GUIs
      "postman"
      "lens"
      "datagrip"
      "rider"
    ] ++ [
      "android-platform-tools"
      "android-file-transfer"

      # aws session manager-plugin 
      "session-manager-plugin"
    ];
  };
}

