{ pkgs, lib, gzpPrivateStuff }:
let
  buildSshCmdProxy = { name, ip }: {
    "gz-${name}" =
      "tmux split-window -h ssh ${name} & tmux split-window -v ssh ${name} && tmux split-window -h ssh ${name} && exit 0";
  };
in {
  enable = true;
  functions = {
    gitignore = "curl -sL https://www.gitignore.io/api/$argv";
    pux = "${pkgs.tmux}/bin/tmux new-session -A -s (pwd).tmux";
    vpn-action = "sudo systemctl $argv[2] wg-quick-$argv[1].service";
  };

  shellInit = ''

    # done notification for teminal commands
      set __done_min_cmd_duration 3000

    # fzf_configure_bindings --help to learn more
      fzf_configure_bindings
    ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source
  '' + lib.optionalString pkgs.stdenv.isDarwin ''
    # incase you need to move the path up
    # fish_add_path -m /run/current-system/sw/bin
    # fish_add_path -m /etc/profiles/per-user/phv/bin/ 
    # need homebrew installs 
    fish_add_path /opt/homebrew/bin
    set -gx AWS_PROFILE wooga-sbs 
    set -gx LDFLAGS "-L/opt/homebrew/opt/ncurses/lib"
    set -gx CPPFLAGS "-I/opt/homebrew/opt/ncurses/include"
  '' +
    # wooga specific variables 
    # TODO: default editor vim there's probably a better way to declare this
    ''
      export SBS_PROJECT_TOOLS_DOCKERLESS=true
      export EDITOR=vim
    '';
  # TODO: https://github.com/zx2c4/password-store/blob/master/src/completion/pass.fish-completion

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
    "..." = "cd ../..";
    "db-local" = "psql -U postgres";
    "db-dev-priv" = ''psql "${gzpPrivateStuff.gzp-dev-psql.urlPrivate}"'';
    "db-dev-public" = ''psql "${gzpPrivateStuff.gzp-dev-psql.urlPublic}"'';

    "lines-count" = "git ls-files | xargs cat | wc -l";

    "l" = "${pkgs.exa}/bin/exa -l";
    "ll" = "${pkgs.exa}/bin/exa -l";
    "ls" = "ll";

    "gz-vpn_on" = "vpn-action gzp-dev start";
    "gz-vpn_off" = "vpn-action gzp-dev stop";
  } // lib.foldl' lib.mergeAttrs { }
    (builtins.map buildSshCmdProxy gzpPrivateStuff.gzp-bastion2machines)
    // lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
      # linux
      "lx-battery" =
        "${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_BAT0";
    };

  # // lib.foldl' lib.mergeAttrs { } (builtins.map buildSshCmdProxy gzpPrivateStuff.gzp-bastion2machines) ;
}
