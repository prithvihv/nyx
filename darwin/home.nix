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

  # tex = with pkgs;
  #   (texlive.combine {
  #     inherit (texlive)
  #       scheme-medium titlesec marvosym xcolor enumitem hyperref fancyhdr
  #       latexmk tlmgrbasics fontawesome;
  #   });
  alacrittyConfig = import ../pkgs/alacritty.nix { };
  tmuxConfig = import ../pkgs/tmux.nix { inherit pkgs; };
  vsCodeConfig = import ../pkgs/vscode.nix { inherit pkgs; };
  zedConfig = import ../pkgs/zed.nix { inherit pkgs; };
  fishConfig = import ../pkgs/fish.nix { inherit pkgs lib; };
  customPkgs = import ../pkgs/nixpkgs { inherit pkgs; };
in {
  home.stateVersion = "23.05";
  # home.enableNixpkgsReleaseCheck = false;
  home.username = "prithvi.virupaksha";
  home.homeDirectory = "/Users/prithvi.virupaksha";

  home.packages = with pkgs;
    [
      go-migrate
      msgpack-tools
      nix-diff
      nixd
      git

      fd
      jq
      fzf
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
      # tex

      pass
      # gnused

      when

      # needed for asdf erlang
      # openjdk17
      # unixODBC
    ] ++ [ git-crypt ]
    ++ [ kubectl kubectx kafkactl aws-iam-authenticator terraform gh ]
    ++ golangTools.extraPkgs ++ nodeTools.extraPkgs ++ haskellTools.extraPkgs
    ++ customPkgs.all ++ elixirTools.extraPkgs;

  programs.bash.enable = false;
  programs.zsh.enable = true;

  programs.alacritty = alacrittyConfig;
  programs.vscode = vsCodeConfig;
  # TODO: zed is marked as broken in nixpkgs unstable, wait till they fix that
  # programs.zed-editor = zedConfig;
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
