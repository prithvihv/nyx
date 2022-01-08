{ pkgs }: {
  extraPkgs = with pkgs; [
    go_1_17
    gcc

    # gotools
    mockgen
    go-tools
    gopls
    go-outline
    gocode
    gopkgs
    gocode-gomod
    godef
    golint
    delve # dlv used in vscode debugging
    go-tools # staticcheck
    go-swag
    go-swagger
  ];
}
