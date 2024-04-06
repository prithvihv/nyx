{ pkgs }:
let priv = import ../priv/priv.nix { };
in {
  enable = true;
  # package = pkgs.vscodium;

  # if extensions stop working try this: https://github.com/nix-community/home-manager/issues/3507#issuecomment-1616803481

  extensions = with pkgs.vscode-extensions;
    let
      lib = pkgs.lib;
      vitesse-theme = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "theme-vitesse";
          publisher = "antfu";
          version = "0.7.6";
          sha256 = "sha256-HNNUltGIgxKRjMMDm3yRzR/sQk/0Tl3RsT9jreWwjJA=";
        };
        meta = with pkgs.lib; {
          description = "Vitesse theme for VS Code";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=antfu.theme-vitesse";
          homepage = "no";
          license = licenses.mit;
        };
      };
      code-stats = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "code-stats-vscode";
          publisher = "riussi";
          version = "1.0.18";
          sha256 = "9UyDK588qpeMWq5OBQwXYl+9qctXIDY3qd/CtosG4TU=";
        };
        meta = with pkgs.lib; {
          description = "Code::Stats";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=riussi.code-stats-vscode";
          homepage = "no";
          license = licenses.mit;
        };
      };
      vscode-jest = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "vscode-jest";
          publisher = "orta";
          version = "5.2.3";
          sha256 = "sha256-cPHwBO7dI44BZJwTPtLR7bfdBcLjaEcyLVvl2Qq+BgE=";
        };
        meta = with pkgs.lib; {
          description =
            "This extension provides an extensible user interface for running your tests in VS Code. It can be used with any testing framework if there is a corresponding Test Adapter extension.";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=hbenl.vscode-test-explorer";
          homepage = "no";
          license = licenses.mit;
        };
      };
      vscode-one-dark-pro = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "material-theme";
          publisher = "zhuangtongfa";
          version = "3.17.0";
          sha256 = "sha256-UZK4ZWTnp4G9ue/GfD6kUw+l50C5ms61aZiXqpYjzr8=";
        };
        meta = with pkgs.lib; {
          description = "";
          downloadPage = "";
          homepage = "no";
          license = licenses.mit;
        };
      };
      nixOsApps =
        pkgs.lib.optionals pkgs.stdenv.isLinux [ ms-vsliveshare.vsliveshare ];
      vscode-test-explorer = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "vscode-test-explorer";
          publisher = "hbenl";
          version = "2.21.1";
          sha256 = "sha256-fHyePd8fYPt7zPHBGiVmd8fRx+IM3/cSBCyiI/C0VAg=";
        };
        meta = with pkgs.lib; {
          description =
            "This extension supports full jest features in vscode environment to make testing more intuitive and fun. It should work out-of-the-box for most common jest projects. ";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=streetsidesoftware.code-spell-checker";
          homepage = "no";
          license = licenses.mit;
        };
      };
      vscode-test-adapter-converter =
        pkgs.vscode-utils.buildVscodeMarketplaceExtension {
          mktplcRef = {
            name = "test-adapter-converter";
            publisher = "ms-vscode";
            version = "0.1.8";
            sha256 = "sha256-ybb3Wud6MSVWEup9yNN4Y4f5lJRCL3kyrGbxB8SphDs=";
          };
          meta = with pkgs.lib; {
            description = "";
            downloadPage =
              "https://marketplace.visualstudio.com/items?itemName=ms-vscode.test-adapter-converter";
            homepage = "no";
            license = licenses.mit;
          };
        };
      vscode-elixir-mix-formatter =
        pkgs.vscode-utils.buildVscodeMarketplaceExtension {
          mktplcRef = {
            name = "vscode-elixir-mix-formatter";
            publisher = "animus-coop";
            version = "1.0.1";
            sha256 = "sha256-wAAU70zVJVNa7boIhniOY4nHBTXSGfRuB5kGtP7NZ/M=";
          };
          meta = with pkgs.lib; {
            description = "";
            downloadPage =
              "https://marketplace.visualstudio.com/items?itemName=animus-coop.vscode-elixir-mix-formatter";
            homepage = "no";
            license = licenses.mit;
          };
        };
    in [
      hashicorp.terraform
      phoenixframework.phoenix
      vscode-test-adapter-converter
      vscode-jest
      vscode-test-explorer
      bbenoist.nix
      brettm12345.nixfmt-vscode
      eamodio.gitlens
      golang.go
      elixir-lsp.vscode-elixir-ls
      bbenoist.nix
      prisma.prisma
      redhat.vscode-yaml
      esbenp.prettier-vscode
      WakaTime.vscode-wakatime
      dracula-theme.theme-dracula
      arrterian.nix-env-selector
      scala-lang.scala
      scalameta.metals
      arrterian.nix-env-selector
      pkief.material-icon-theme
      # ms-vscode.cpptools
      # c-cpp-runner
      ms-vscode-remote.remote-ssh
      redhat.java
      vscodevim.vim
      haskell.haskell
      james-yu.latex-workshop

      dbaeumer.vscode-eslint
      streetsidesoftware.code-spell-checker
      eamodio.gitlens
      james-yu.latex-workshop
      redhat.vscode-yaml
      foxundermoon.shell-format

      vitesse-theme
      vscode-one-dark-pro
      code-stats
      vscode-elixir-mix-formatter
    ] ++ nixOsApps;

  enableUpdateCheck = false;
  enableExtensionUpdateCheck = false;

  userSettings = {
    "latex-workshop.kpsewhich.path" =
      "/etc/profiles/per-user/phv/bin/kpsewhich";
    "git.path" = "/etc/profiles/per-user/phv/bin/git";
    "latex-workshop.latex.tools" = [{
      "name" = "latexmk";
      "command" = "latexmk";
      "args" = [
        "-xelatex"
        "-synctex=1"
        "-interaction=nonstopmode"
        "-file-line-error"
        "%DOC%"
      ];
    }];
    "go.coverOnSave" = true;
    "go.coverageDecorator" = {
      "type" = "gutter";
      "coveredHighlightColor" = "rgba(64,128,128,0.5)";
      "uncoveredHighlightColor" = "rgba(128,64,64,0.25)";
      "coveredGutterStyle" = "blockgreen";
      "uncoveredGutterStyle" = "blockred";
    };
    "go.coverOnSingleTest" = true;

    "workbench.colorTheme" = "One Dark Pro Darker";
    # "workbench.colorTheme" = "Vitesse Light";
    # "workbench.colorTheme" = "Dracula";
    "workbench.editor.enablePreview" = false;
    "vetur.format.defaultFormatterOptions" = {
      "js-beautify-html" = { "wrap_attributes" = "aligned-multiple"; };
      "prettyhtml" = {
        "printWidth" = 400;
        "wrapAttributes" = false;
      };
    };
    "workbench.sideBar.location" = "left";
    "editor.lineNumbers" = "relative";
    "editor.fontLigatures" = true;
    "vim.handleKeys" = {
      "<C-f>" = false;
      "<C-w>" = false;
      "<C-s>" = false;
      "<C-b>" = false;
      "<C-c>" = false;
      "<C-d>" = false;
      "<C-x>" = false;
      "<C-p>" = false;
    };

    "editor.fontFamily" =
      "JetBrains Mono,  Iosevka, FuraCode Nerd Font, Material Icons";
    "editor.fontSize" = 11;
    "javascript.updateImportsOnFileMove.enabled" = "always";
    "diffEditor.ignoreTrimWhitespace" = false;
    "go.formatTool" = "goimports";
    "go.lintFlags" =
      [ "--disable-all" "--enable=golint" "--config=~/.gometalinter" ];
    "explorer.confirmDelete" = false;
    "explorer.confirmDragAndDrop" = false;
    "workbench.iconTheme" = "vscode-icons";
    "files.watcherExclude" = {
      "**/.bloop" = true;
      "**/.metals" = true;
      "**/.ammonite" = true;
    };
    "window.menuBarVisibility" = "toggle";
    "[javascript]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "workbench.editorAssociations" = { "*.ipynb" = "jupyter-notebook"; };
    "redhat.telemetry.enabled" = false;
    "[yaml]" = { "editor.defaultFormatter" = "redhat.vscode-yaml"; };
    "[json]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "[html]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "[prisma]" = { "editor.defaultFormatter" = "Prisma.prisma"; };
    "[clojure]" = {
      "editor.autoClosingBrackets" = "always";
      "editor.autoClosingOvertype" = "always";
      "editor.formatOnPaste" = true;
    };
    "[typescript]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "[typescriptreact]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[jsonc]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "[nix]" = { "editor.defaultFormatter" = "brettm12345.nixfmt-vscode"; };
    "[dockercompose]" = {
      "editor.defaultFormatter" = "ms-azuretools.vscode-docker";
    };
    "[elixir]" = {
      "editor.defaultFormatter" = "animus-coop.vscode-elixir-mix-formatter";
    };
    "[css]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "json.schemas" = [ ];
    "go.toolsGopath" = "/etc/profiles/per-user/phv/bin/";
    "cSpell.userWords" = [
      "alch"
      "apay"
      "arvind"
      "arvindpunk"
      "davecgh"
      "gamezop"
      "gock"
      "gomock"
      "GZPID"
      "Ifsc"
      "jinbe"
      "jlog"
      "jmoiron"
      "kelasa"
      "normie"
      "rabbitmq"
      "sqlx"
      "stretchr"
      "Strs"
      "txns"
      "upsert"
      "Zoro"
      "ecto"
      "errr"
      "requestid"
      "Avble"
      "Backends"
      "Bidbook"
      "Connx"
      "Frontends"
      "GOPATH"
      "Idempotency"
      "MATCHMOVE"
      "Msgf"
      "Quizzop"
      "Recurr"
      "Rowx"
      "TRANSACTIONTYPEPRD"
      "Txns"
      "Withdrable"
      "acked"
      "alch"
      "amqp"
      "cmds"
      "defmemo"
      "ecto"
      "errorh"
      "errr"
      "fundingsocieties"
      "fundingsocietiesdocker"
      "gamehistory"
      "godotenv"
      "gomock"
      "goveralls"
      "idempotency"
      "jmoiron"
      "kelasa"
      "keyscan"
      "luffy"
      "mrkdwn"
      "mult"
      "noout"
      "notok"
      "paytm"
      "psql"
      "razorpay"
      "requestid"
      "rzpay"
      "sharded"
      "sqlx"
      "unacked"
      "unmarshalled"
      "usopp"
      "venky"
      "zerolog"
      "Detial"
      "gonic"
      "Kolkata"
      "LDBTYPE"
      "Leaderboad"
      "luffy"
      "przs"
      "sequencially"
      "sanevalidators"
    ];
    "aws.profile" = "profile=default";
    "window.zoomLevel" = 1;

    "calva.clojureLspPath" = "/etc/profiles/per-user/phv/bin/clojure-lsp";
    "java.jdt.ls.java.home" = "${pkgs.openjdk17}/lib/openjdk";
    # "haskell.serverExecutablePath" = "${pkgs.haskell-language-server}/";

    "codestats.apikey" = priv.code-stat-skadi-api;
    "codestats.apiurl" = "https://codestats.net/api/";
    "codestats.username" = "phv";

    "java.configuration.runtimes" = [{
      name = "JavaSE-17";
      path = "${pkgs.openjdk17}/lib/openjdk";
      default = true;
    }];

    "gitlens.ai.experimental.provider" = "gpt-4-1106-preview";
  };
}
