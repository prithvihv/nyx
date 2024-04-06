{ config, pkgs, lib, ... }:
let
  golangTools = import ../pkgs/languages/golang.nix { inherit lib pkgs; };
  nodeTools = import ../pkgs/languages/node/node.nix {
    inherit pkgs;
    includePrismaTools = false;
  };
  elixirTools = import ../pkgs/languages/elixir.nix { inherit pkgs; };
  haskellTools = import ../pkgs/languages/haskell.nix { inherit pkgs; };
  gitConfig = import ../pkgs/git.nix { inherit pkgs; };
  vimConfig = import ../pkgs/vim.nix { inherit pkgs; };

  tex = with pkgs;
    (texlive.combine {
      inherit (texlive)
        scheme-medium titlesec marvosym xcolor enumitem hyperref fancyhdr
        latexmk tlmgrbasics fontawesome;
    });
  alacrittyConfig = import ../pkgs/alacritty.nix { };
  tmuxConfig = import ../pkgs/tmux.nix { inherit pkgs; };
  vsCodeConfig = import ../pkgs/vscode.nix { inherit pkgs; };
  fishConfig = import ../pkgs/fish.nix { inherit pkgs lib; };
  customPkgs = import ../pkgs/nixpkgs { inherit pkgs; };
in {
  home.stateVersion = "23.05";
  # home.enableNixpkgsReleaseCheck = false;
  home.username = "phv";
  home.homeDirectory = "/Users/phv";

  home.packages = with pkgs;
    [
      go-migrate
      msgpack-tools
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

      openjdk17

      pandoc
      tex

      pass
      # gnused

      # needed for asdf erlang
      # openjdk17
      # unixODBC
    ] ++ [ git-crypt ] ++ golangTools.extraPkgs ++ nodeTools.extraPkgs
    ++ haskellTools.extraPkgs ++ customPkgs.all ++ elixirTools.extraPkgs;

  programs.bash.enable = false;
  programs.zsh.enable = true;

  programs.alacritty = alacrittyConfig;
  programs.vscode = vsCodeConfig;
  programs.tmux = tmuxConfig;
  programs.fish = fishConfig;
  programs.ssh = {
    enable = true;
    matchBlocks = { };
  };
  programs.git = gitConfig // {
    userName = "prithvihv-wooga";
    userEmail = "prithvi.virupaksha@wooga.net";
  };
  programs.neovim = vimConfig;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
