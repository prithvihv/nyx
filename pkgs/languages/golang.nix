{ pkgs }: {
  extraPkgs = with pkgs; [
    go_1_17
    gcc
    go-protobuf
    protobuf
    # protoc-gen-go

    # gotools
    mockgen
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
    gotests
    gotools

    graphviz # pprof golang needs this
  ];
}
