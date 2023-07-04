{ config, pkgs, lib, ... }:
let
  gzpPrivateStuff = import ../priv/gzp-stuff.nix { inherit lib; };
  golangTools = import ../pkgs/languages/golang.nix { inherit lib pkgs; };
  nodeTools = import ../pkgs/languages/node/node.nix {
    inherit pkgs;
    Security = pkgs.darwin.apple_sdk.frameworks.Security;
  };
  elixirTools = import ../pkgs/languages/elixir.nix { inherit pkgs; };
  haskellTools = import ../pkgs/languages/haskell.nix { inherit pkgs; };

  tex = with pkgs;
    (texlive.combine {
      inherit (texlive)
        scheme-medium titlesec marvosym xcolor enumitem hyperref fancyhdr
        latexmk tlmgrbasics fontawesome;
    });
  alacrittyConfig = import ../pkgs/alacritty.nix { };
  tmuxConfig = import ../pkgs/tmux.nix { inherit pkgs; };
  vsCodeConfig = import ../pkgs/vscode.nix { inherit gzpPrivateStuff pkgs; };
  fishConfig = import ../pkgs/fish.nix { inherit pkgs lib gzpPrivateStuff; };
in {
  home.stateVersion = "22.11";
  home.username = "phv";
  home.homeDirectory = "/Users/phv";

  # TODO: this is not working
  home.packages = with pkgs;
    [
      go-migrate
      nix-diff
      git

      fd
      jq
      codespell
      gnumake
      pgsync
      unzip
      nixfmt
      nodePackages.serve
      awscli2
      wakatime
      unp
      neovide

      pandoc
      tex

      pass
    ] ++ [ git-crypt ] ++ golangTools.extraPkgs ++ nodeTools.extraPkgs
    ++ elixirTools.extraPkgs ++ haskellTools.extraPkgs;

  programs.bash.enable = true;
  programs.zsh.enable = true;

  programs.alacritty = alacrittyConfig;
  programs.vscode = vsCodeConfig;
  programs.tmux = tmuxConfig;
  programs.fish = fishConfig;
  programs.ssh = {
    enable = true;
    matchBlocks = { } // gzpPrivateStuff.gzp-ssh;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
