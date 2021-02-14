{ pkgs }:
let
in {
  enable = true;
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;

  extraPackages = with pkgs;[
    git
    vimPlugins.vim-plug
  ];

  configure  = {
    
    customRC = builtins.readFile ./config.vim ;

    plug.plugins = with pkgs.vimPlugins; [
      # misc
      dracula-vim
      YouCompleteMe
      vim-wakatime
      vim-watchdogs
      vim-lastplace
    ] ++ [
      # language support
      rust-vim
      elm-vim
      vim-go
      vim-nix
      vim-elixir
      vim-fish
      vim-javascript
    ] ++ [
      # data representations
      vim-json
      vim-yaml
    ];
  };
}
