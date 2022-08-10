{ pkgs, lib }:
let
  sqlc = with pkgs;
    buildGo118Module rec {
      pname = "sqlc";
      version = "1.14.0";

      src = fetchFromGitHub {
        owner = "kyleconroy";
        repo = "sqlc";
        rev = "v${version}";
        sha256 = "sha256-+JkNuN5Hv1g1+UpJEBZpf7QV/3A85IVzMa5cfeRSQRo=";
      };
      subPackages = [ "cmd/sqlc" ];

      # need these clibs
      doCheck = false;
      vendorSha256 = "sha256-Dr2egNtNQTImO9pjpignR7W9HJHvXnPmZz2QBAMpPs8=";
      buildInputs = [ libpg_query libpqxx xxHash postgresql_14 unzip ];
      proxyVendor = true;

      meta = with lib; {
        description = "Generate type-safe code from SQL";
        homepage = "https://sqlc.dev/";
      };
    };
  sqlc_13 = with pkgs;
    buildGoModule rec {
      pname = "sqlc";
      version = "1.13.0";

      src = fetchFromGitHub {
        owner = "kyleconroy";
        repo = "sqlc";
        rev = "v${version}";
        sha256 = "sha256-HPCt47tctVV8Oz9/7AoVMezIAv6wEsaB7B4rgo9/fNU=";
      };

      proxyVendor = true;
      vendorSha256 = "sha256-zZ0IrtfQvczoB7th9ZCUlYOtyZr3Y3yF0pKzRCqmCjo=";

      subPackages = [ "cmd/sqlc" ];

      meta = let inherit (lib) licenses platforms maintainers;
      in {
        description = "Generate type-safe code from SQL";
        homepage = "https://sqlc.dev/";
        license = licenses.mit;
        platforms = platforms.all;
        maintainers = [ maintainers.adisbladis ];
      };
    };
in {
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

    # clis
    # sqlc
    sqlc_13
  ];
}
