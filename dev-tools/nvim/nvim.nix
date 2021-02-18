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

  package = pkgs.neovim-nightly;

  extraConfig = builtins.readFile ./config.vim ;

  plugins = with pkgs.vimPlugins; [
    # misc
    dracula-vim # theme
    YouCompleteMe
    # FIXME: rplugin.vim issue with this package!
    # vim-wakatime # waka time log 
    vim-watchdogs # watch and correct code
    vim-lastplace # spawn cursor at last edited location
    vim-over # view text file serach and replace
  ] ++  [
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
}
