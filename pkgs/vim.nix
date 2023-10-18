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
    neoscroll-nvim

    # auto completes
    nvim-cmp
    cmp-nvim-lsp
    cmp-vsnip

    lspkind-nvim

    telescope-nvim
    nvim-treesitter

    dracula-vim

    nix-develop-nvim
    vim-elixir
    vim-go
  ];

  extraConfig = pkgs.lib.readFile ./vimrc;
}
