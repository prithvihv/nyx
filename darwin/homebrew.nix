{ config, pkgs, lib, ... }: {
  homebrew = {
    enable = true;
    onActivation.upgrade = true;
    brews = [ "saml2aws" "asdf" ];
    # updates homebrew packages on activation,
    # can make darwin-rebuild much slower (otherwise i'd forget to do it ever though)
    casks = [ "gpg-suite" ] ++ [
      "vlc"
      "signal"
      "visual-studio-code"
      "chromium"
      "spotify"
      "qtpass"
    ] ++ [
      # productivity
      "jumpcut"
      "amethyst"
      "anki"
      "logseq"
    ] ++ [ 
      # devtools GUIs
      "postman" 
      "lens" 
      "datagrip"
    ] ++ [
      "android-platform-tools"
      "android-file-transfer"

      # aws session manager-plugin 
      "session-manager-plugin" 
    ];
  };
}
