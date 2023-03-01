{ pkgs }:
let
  nodeEnv = pkgs.callPackage ./node-env.nix { nodejs = pkgs.nodejs-14_x; };

  # Updating this package will force an update for nodePackages.prisma. The
  # version of prisma-engines and nodePackages.prisma must be the same for them to
  # function correctly.
  prisma-engine = with pkgs;
    rustPlatform.buildRustPackage rec {
      pname = "prisma-engines";
      version = "4.10.1";

      src = fetchFromGitHub {
        owner = "prisma";
        repo = "prisma-engines";
        rev = version;
        sha256 = "sha256-TFLwpKh+FsstcpvBfTw7CNcQOGGSNI9qf8WJ6v75uL8=";
      };

      # Use system openssl.
      OPENSSL_NO_VENDOR = 1;

      cargoSha256 = "sha256-EPym9MLwTMGBbJkVMKD/NEc6Vfm7nI4FaDkqy/0B14Q=";

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
  prisma = with pkgs;
    nodeEnv.buildNodePackage {
      name = "prisma";
      packageName = "prisma";
      version = "4.10.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/prisma/-/prisma-4.10.1.tgz";
        sha512 =
          "sha512-0jDxgg+DruB1kHVNlcspXQB9au62IFfVg9drkhzXudszHNUAQn0lVuu+T8np0uC2z1nKD5S3qPeCyR8u5YFLnA==";
      };
      dependencies = [{
        name = "_at_prisma_slash_engines";
        packageName = "@prisma/engines";
        version = "4.10.1";
        src = fetchurl {
          url =
            "https://registry.npmjs.org/@prisma/engines/-/engines-4.10.1.tgz";
          sha512 =
            "sha512-B3tcTxjx196nuAu1GOTKO9cGPUgTFHYRdkPkTS4m5ptb2cejyBlH9X7GOfSt3xlI7p4zAJDshJP4JJivCg9ouA==";
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
    nodejs
    yarn
    # prisma-engines 
    # nodePackages.prisma
    prisma
    prisma-engine
  ];
}
