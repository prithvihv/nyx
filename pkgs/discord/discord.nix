self: super:
let
  branch = "stable";
  pkgs = self;
  lib = pkgs.lib;
  stdenv = pkgs.stdenv;
  inherit (pkgs) callPackage fetchurl;

  # copy the below part from: https://github.com/NixOS/nixpkgs/blob/nixos-22.05/pkgs/applications/networking/instant-messengers/discord/default.nix
   versions = if stdenv.isLinux then {
    stable = "0.0.22";
    ptb = "0.0.35";
    canary = "0.0.143";
  } else {
    stable = "0.0.264";
    ptb = "0.0.59";
    canary = "0.0.283";
  };
  version = versions.${branch};
  srcs = rec {
    x86_64-linux = {
      stable = fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "F1xzdx4Em6Ref7HTe9EH7whx49iFc0DFpaQKdFquq6c=";
      };
      ptb = fetchurl {
        url = "https://dl-ptb.discordapp.net/apps/linux/${version}/discord-ptb-${version}.tar.gz";
        sha256 = "bnp5wfcR21s7LMPxFgj5G3UsxPWlFj4t6CbeosiufHY=";
      };
      canary = fetchurl {
        url = "https://dl-canary.discordapp.net/apps/linux/${version}/discord-canary-${version}.tar.gz";
        sha256 = "sha256-K+yyg9GTAvggfn4JQCTmq016tMyyzq+nD7aL3+bWFlo=";
      };
    };
    x86_64-darwin = {
      stable = fetchurl {
        url = "https://dl.discordapp.net/apps/osx/${version}/Discord.dmg";
        sha256 = "1jvlxmbfqhslsr16prsgbki77kq7i3ipbkbn67pnwlnis40y9s7p";
      };
      ptb = fetchurl {
        url = "https://dl-ptb.discordapp.net/apps/osx/${version}/DiscordPTB.dmg";
        sha256 = "sha256-LS7KExVXkOv8O/GrisPMbBxg/pwoDXIOo1dK9wk1yB8=";
      };
      canary = fetchurl {
        url = "https://dl-canary.discordapp.net/apps/osx/${version}/DiscordCanary.dmg";
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
