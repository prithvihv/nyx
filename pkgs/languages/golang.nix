{ pkgs }: {
  extraPkgs = with pkgs; [
    go_1_17
    gotools
    gopls
    go-outline
    gocode
    gopkgs
    gocode-gomod
    godef
    golint
    delve # dlv used in vscode debugging
    go-tools # staticcheck
  ];
}
