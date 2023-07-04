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

    ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source

    fish_add_path -m /run/current-system/sw/bin
    fish_add_path -m /etc/profiles/per-user/phv/bin/ 
  '';
  # TODO: https://github.com/zx2c4/password-store/blob/master/src/completion/pass.fish-completion

  plugins = [
    {
      name = "theme-batman";
      src = pkgs.fetchFromGitHub {
        owner = "oh-my-fish";
        repo = "theme-batman";
        rev = "2a76bd81f4805debd7f137cb98828bff34570562";
        sha256 = "Ko4w9tMnIi17db174FzW44LgUdui/bUzPFEHEHv//t4=";
      };
    }
    {
      # not working
      name = "plugin-pbcopy";
      src = pkgs.fetchFromGitHub {
        owner = "oh-my-fish";
        repo = "plugin-pbcopy";
        rev = "e8d78bb01f66246f7996a4012655b8ddbad777c2";
        sha256 = "sha256-B6/0tNk5lb+1nup1dfXhPD2S5PURZyFd8nJJF6shvq4=";
      };
    }
    {
      name = "done";
      src = pkgs.fetchFromGitHub {
        owner = "franciscolourenco";
        repo = "done";
        rev = "d6abb267bb3fb7e987a9352bc43dcdb67bac9f06";
        sha256 = "6oeyN9ngXWvps1c5QAUjlyPDQwRWAoxBiVTNmZ4sG8E=";
      };
    }
  ];

  shellAbbrs = {
    gco = "git checkout";
    gc = "git commit -m";
    gl = "git log";
    ga = "git add";
    gpl = "git pull";
    gp = "git push";
    cb = "pbcopy";

    kk = "sudo k3s kubectl";
  };

  shellAliases = {
    "..." = "cd ../..";
    "db-local" = "psql -U postgres";
    "db-dev-priv" = ''psql "${gzpPrivateStuff.gzp-dev-psql.urlPrivate}"'';
    "db-dev-public" = ''psql "${gzpPrivateStuff.gzp-dev-psql.urlPublic}"'';

    "lines-count" = "git ls-files | xargs cat | wc -l";

    "cd-hdd_sw" = "cd /run/media/phv/bf8da584-7bf2-425c-a602-8b3b997814d0";
    "cd-hdd_data" = "cd /run/media/phv/d930b0b1-853e-45d7-b249-f71f7108b3ac";
    "cd-gz" = "cd /home/phv/code/src/github.com/gamezop";
    "cd-config" = "cd /home/phv/.nyx";
    "cd-fzf" = "cd (${pkgs.fd}/bin/fd --type directory | fzf)"; # ALT-C
    "code-ed" =
      "cd /home/phv/code/src/github.com/prithvihv/ed/groww-scraper && source ~/.secret.sh && code .";

    "l" = "${pkgs.exa}/bin/exa";
    "ll" = "${pkgs.exa}/bin/exa -l";
    "ls" = "l";

    "gz-j" = "ssh gz_jump";
    "gz-vpn_on" = "vpn-action gzp-dev start";
    "gz-vpn_off" = "vpn-action gzp-dev stop";
    # "gz-coke" = "tmux split-window -h ssh rbac-coke-new & tmux split-window -v ssh rbac-coke-new && tmux split-window -h ssh rbac-coke-new && exit 0";

    "lx-k-taffy" = "/home/phv/.nyx/users/phv/cron/killtaffy.sh";
  } // lib.foldl' lib.mergeAttrs { }
    (builtins.map buildSshCmdProxy gzpPrivateStuff.gzp-bastion2machines)
    // lib.attrsets.optionalAttrs pkgs.stdenv.isLinux {
      # linux
      "lx-battery" =
        "${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_BAT0";
    };

  # // lib.foldl' lib.mergeAttrs { } (builtins.map buildSshCmdProxy gzpPrivateStuff.gzp-bastion2machines) ;
}
