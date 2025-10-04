{ pkgs, lib, isWoogaMachine }:
let
in {
  enable = true;
  functions = {
    gitignore = "curl -sL https://www.gitignore.io/api/$argv";
    pux = "${pkgs.tmux}/bin/tmux new-session -A -s (pwd).tmux";
  };

  shellInit = ''
    # done notification for teminal commands
      set __done_min_cmd_duration 3000

    # fzf_configure_bindings --help to learn more
      fzf_configure_bindings
    ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source
  '' + lib.optionalString (pkgs.stdenv.isDarwin && isWoogaMachine) ''
    # incase you need to move the path up
    # fish_add_path -m /run/current-system/sw/bin
    # fish_add_path -m /etc/profiles/per-user/phv/bin/
    # need homebrew installs
    fish_add_path /opt/homebrew/bin
    set -gx AWS_PROFILE wooga-sbs
    export SBS_PROJECT_TOOLS_DOCKERLESS=true
    export EDITOR=vim
    source "$HOME/.cargo/env.fish"
  '';
  plugins = let
    custom = [{
      name = "theme-batman";
      src = pkgs.fetchFromGitHub {
        owner = "oh-my-fish";
        repo = "theme-batman";
        rev = "2a76bd81f4805debd7f137cb98828bff34570562";
        sha256 = "Ko4w9tMnIi17db174FzW44LgUdui/bUzPFEHEHv//t4=";
      };
    }];
  in with pkgs.fishPlugins;
  [
    # https://nixos.wiki/wiki/Fish
    {
      name = "done";
      src = done.src;
    }
    {
      name = "fzf-fish";
      src = fzf-fish.src;
    }
  ] ++ custom;

  shellAbbrs = {
    gco = "git checkout";
    gc = "git commit -m";
    gl = "git log";
    ga = "git add";
    gpl = "git pull";
    gp = "git push";
    # TODO: write a min scipt for this to handle mac also
    cb = "${pkgs.xclip}/bin/xclip -selection clipboard";
  };

  shellAliases = {
    "k" = "kubectl";
    "t" = "terraform";
    "..." = "cd ../..";
    "db-local" = "psql -U postgres";
    "lines-count" = "git ls-files | xargs cat | wc -l";

    "l" = "${pkgs.eza}/bin/eza -l";
    "ll" = "${pkgs.eza}/bin/eza -l";
    "ls" = "ll";

    "gz-vpn_on" = "vpn-action gzp-dev start";
    "gz-vpn_off" = "vpn-action gzp-dev stop";
  } // lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
    # linux
    "lx-battery" =
      "${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_BAT0";
  };
}
