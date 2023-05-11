self: super:
let
  branch = "stable";
  pkgs = self;
  lib = pkgs.lib;
  stdenv = pkgs.stdenv;
  inherit (pkgs) callPackage fetchurl;

  # copy the below part from: https://github.com/NixOS/nixpkgs/blob/nixos-22.11/pkgs/applications/networking/instant-messengers/discord/default.nix
  versions = if stdenv.isLinux then {
    stable = "0.0.27";
    ptb = "0.0.42";
    canary = "0.0.151";
  } else {
    stable = "0.0.273";
    ptb = "0.0.59";
    canary = "0.0.283";
  };
  version = versions.${branch};
  srcs = rec {
    x86_64-linux = {
      stable = fetchurl {
        url =
          "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "sha256-6fHaiPBcv7TQVh+TatIEYXZ/LwPmnCmU/QWXKFgUR7U=";
      };
      ptb = fetchurl {
        url =
          "https://dl-ptb.discordapp.net/apps/linux/${version}/discord-ptb-${version}.tar.gz";
        sha256 = "ZAMyAqyFEBJeTUqQzr5wK+BOFGURqhoHL8w2hJvL0vI=";
      };
      canary = fetchurl {
        url =
          "https://dl-canary.discordapp.net/apps/linux/${version}/discord-canary-${version}.tar.gz";
        sha256 = "sha256-ZN+lEGtSajgYsyMoGRmyTZCpUGVmb9LKgVv89NA4m7U=";
      };
    };
    x86_64-darwin = {
      stable = fetchurl {
        url = "https://dl.discordapp.net/apps/osx/${version}/Discord.dmg";
        sha256 = "1vz2g83gz9ks9mxwx7gl7kys2xaw8ksnywwadrpsbj999fzlyyal";
      };
      ptb = fetchurl {
        url =
          "https://dl-ptb.discordapp.net/apps/osx/${version}/DiscordPTB.dmg";
        sha256 = "sha256-LS7KExVXkOv8O/GrisPMbBxg/pwoDXIOo1dK9wk1yB8=";
      };
      canary = fetchurl {
        url =
          "https://dl-canary.discordapp.net/apps/osx/${version}/DiscordCanary.dmg";
        sha256 = "0mqpk1szp46mih95x42ld32rrspc6jx1j7qdaxf01whzb3d4pi9l";
      };
    };
    aarch64-darwin = x86_64-darwin;
  };
  src = srcs.${stdenv.hostPlatform.system}.${branch};

  meta = with lib; {
    description = "All-in-one cross-platform voice and text chat for gamers";
    homepage = "https://discordapp.com/";
    downloadPage = "https://discordapp.com/download";
    license = licenses.unfree;
    maintainers = with maintainers; [ ldesgoui MP2E devins2518 ];
    platforms = [ "x86_64-linux" "x86_64-darwin" ]
      ++ lib.optionals (branch == "ptb") [ "aarch64-darwin" ];
  };
  package = if stdenv.isLinux then ./linux.nix else ./darwin.nix;
  packages = (builtins.mapAttrs (_: value:
    callPackage package (value // {
      inherit src version;
      meta = meta // { mainProgram = value.binaryName; };
    })) {
      stable = rec {
        pname = "discord";
        binaryName = "Discord";
        desktopName = "Discord";
      };
      ptb = rec {
        pname = "discord-ptb";
        binaryName = "DiscordPTB";
        desktopName = "Discord PTB";
      };
      canary = rec {
        pname = "discord-canary";
        binaryName = "DiscordCanary";
        desktopName = "Discord Canary";
      };
    });
in with super; { discord = packages.${branch}; }
