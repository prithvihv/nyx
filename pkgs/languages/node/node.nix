{ pkgs, includePrismaTools ? true }:
let
in {
  /* export PRISMA_MIGRATION_ENGINE_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/migration-engine"
     export PRISMA_QUERY_ENGINE_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/query-engine"
     export PRISMA_QUERY_ENGINE_LIBRARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/lib/libquery_engine.node"
     export PRISMA_INTROSPECTION_ENGINE_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/introspection-engine"
     export PRISMA_FMT_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/prisma-fmt"
  */

  extraPkgs = with pkgs;
    let
      prismaTools =
        if includePrismaTools then [ prisma-engines ] else [ ];
    in [
      nodejs_latest
      yarn
      # nodePackages.prisma
    ] ++ prismaTools;
}
