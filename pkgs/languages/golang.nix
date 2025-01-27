{ pkgs, lib }:
let
in {
  extraPkgs = with pkgs; [
    go
    # gcc # issue with macOS, they use clang
    go-protobuf
    protobuf
    # protoc-gen-go

    # gotools
    mockgen
    gopls
    go-outline
    gopkgs
    godef
    golint
    delve # dlv used in vscode debugging
    go-tools # staticcheck
    go-swag
    go-swagger
    gotests
    gotools # ruby_3 conflicts

    graphviz # pprof golang needs this

    # clis
    sqlc
  ];
}
