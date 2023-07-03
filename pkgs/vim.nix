{ pkgs }:
let
in {
  enable = true;
  # package = pkgs.neovim;
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;

  plugins = with pkgs.vimPlugins; [
    nvim-lspconfig
    nvim-notify
    nvim-peekup
    nvim-scrollview

    # auto completes
    nvim-cmp
    cmp-nvim-lsp
    cmp-vsnip
    vim-vsnip

    lspkind-nvim

    telescope-nvim
    nvim-treesitter

    dracula-vim

    vim-nix
    vim-elixir
    # alchemist
    vim-go
  ];

  extraConfig = pkgs.lib.readFile ./vimrc;
}
