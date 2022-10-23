{ pkgs }:
let
  nodeEnv = pkgs.callPackage ./node-env.nix { nodejs = pkgs.nodejs-14_x; };

  # Updating this package will force an update for nodePackages.prisma. The
  # version of prisma-engines and nodePackages.prisma must be the same for them to
  # function correctly.
  prisma-engine-4-3-1 = with pkgs;
    rustPlatform.buildRustPackage rec {
      pname = "prisma-engines";
      version = "4.2.1";

      src = fetchFromGitHub {
        owner = "prisma";
        repo = "prisma-engines";
        rev = version;
        sha256 = "sha256-TlKjAfpygQq2c77d6ZoMIBtWC0bAiMiKygFkh5GrBBc=";
      };

      # Use system openssl.
      OPENSSL_NO_VENDOR = 1;

      cargoSha256 = "sha256-KkCq7h6qqh37LvA4wQYjLk/LPKCg5Wgl6tEhH55qh8M=";

      nativeBuildInputs = [ pkg-config ];

      buildInputs = [ openssl protobuf ]
        ++ lib.optionals stdenv.isDarwin [ Security ];

      preBuild = ''
        export OPENSSL_DIR=${lib.getDev openssl}
        export OPENSSL_LIB_DIR=${lib.getLib openssl}/lib
        export PROTOC=${protobuf}/bin/protoc
        export PROTOC_INCLUDE="${protobuf}/include";
        export SQLITE_MAX_VARIABLE_NUMBER=250000
        export SQLITE_MAX_EXPR_DEPTH=10000
      '';

      cargoBuildFlags =
        "-p query-engine -p query-engine-node-api -p migration-engine-cli -p introspection-core -p prisma-fmt";

      postInstall = ''
        mv $out/lib/libquery_engine${stdenv.hostPlatform.extensions.sharedLibrary} $out/lib/libquery_engine.node
      '';

      # Tests are long to compile
      doCheck = false;

      meta = with lib; {
        description =
          "A collection of engines that power the core stack for Prisma";
        homepage = "https://www.prisma.io/";
        license = licenses.asl20;
        platforms = platforms.unix;
        maintainers = with maintainers; [
          pamplemousse
          pimeys
          superherointj
          tomhoule
        ];
      };
    };
  prisma-4-3-1 = with pkgs;
    nodeEnv.buildNodePackage {
      name = "prisma";
      packageName = "prisma";
      version = "4.3.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/prisma/-/prisma-4.3.1.tgz";
        sha512 =
          "90xo06wtqil76Xsi3mNpc4Js3SdDRR5g4qb9h+4VWY4Y8iImJY6xc3PX+C9xxTSt1lr0Q89A0MLkJjd8ax6KiQ==";
      };
      dependencies = [{
        name = "_at_prisma_slash_engines";
        packageName = "@prisma/engines";
        version = "4.3.1";
        src = fetchurl {
          url =
            "https://registry.npmjs.org/@prisma/engines/-/engines-4.3.1.tgz";
          sha512 =
            "4JF/uMaEDAPdcdZNOrnzE3BvrbGpjgV0FcPT3EVoi6I86fWkloqqxBt+KcK/+fIRR0Pxj66uGR9wVH8U1Y13JA==";
        };
      }];
      buildInputs = [ ];
      meta = {
        description =
          "Prisma is an open-source database toolkit. It includes a JavaScript/TypeScript ORM for Node.js, migrations and a modern GUI to view and edit the data in your database. You can use Prisma in new projects or add it to an existing one.";
        homepage = "https://www.prisma.io";
        license = "Apache-2.0";
      };
      production = true;
      bypassCache = true;
      reconstructLock = true;
    };
in {
  /* export PRISMA_MIGRATION_ENGINE_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/migration-engine"
     export PRISMA_QUERY_ENGINE_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/query-engine"
     export PRISMA_QUERY_ENGINE_LIBRARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/lib/libquery_engine.node"
     export PRISMA_INTROSPECTION_ENGINE_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/introspection-engine"
     export PRISMA_FMT_BINARY="/nix/store/z6b2myc9bhgb94d3140hzww50d5y1csf-prisma-engines-4.2.1/bin/prisma-fmt"
  */

  extraPkgs = with pkgs; [
    nodejs-16_x
    yarn
    # prisma-engines 
    # nodePackages.prisma
    prisma-4-3-1
    prisma-engine-4-3-1
  ];
}
