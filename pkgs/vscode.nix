{ pkgs, gzpPrivateStuff }: {
  enable = true;
  # package = pkgs.vscodium;
  extensions = with pkgs.vscode-extensions;
    let
      lib = pkgs.lib;
      vitesse-theme = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "theme-vitesse";
          publisher = "antfu";
          version = "0.6.0";
          sha256 = "sha256-LgNJzRBU1+UxL61LkghSFzbUNtBTh1cVdfaSCNkrymk=";
        };
        meta = with pkgs.lib; {
          description = "Vitesse theme for VS Code";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=antfu.theme-vitesse";
          homepage = "no";
          license = licenses.mit;
        };
      };
      jest = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "vscode-jest";
          publisher = "orta";
          version = "5.2.0";
          sha256 = "E9VwEPclxLi33Y9HfMPnLqGQWZecLajCf3rCoKw89pQ=";
        };
        meta = with pkgs.lib; {
          description = "Code::Stats";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=orta.vscode-jest";
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
      makefile-tools = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "makefile-tools";
          publisher = "ms-vscode";
          version = "0.5.0";
          sha256 = "oBYABz6qdV9g7WdHycL1LrEaYG5be3e4hlo4ILhX4KI=";
        };
        meta = with pkgs.lib; {
          description = "Makefile Tools";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=ms-vscode.makefile-tools";
          homepage = "no";
          license = licenses.mit;
        };
      };
      c-cpp-runner = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "c-cpp-runner";
          publisher = "franneck94";
          version = "4.0.4";
          sha256 = "mJUxSAp2SkmLYtE/VQJfKy84k0qo0YoxBIzFY+a5CSI=";
        };
        meta = with pkgs.lib; {
          description = "Makefile Tools";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=franneck94.c-cpp-runner";
          homepage = "no";
          license = licenses.mit;
        };
      };
      nixOsApps =
        pkgs.lib.optionals pkgs.stdenv.isLinux [ ms-vsliveshare.vsliveshare ];
    in [
      bbenoist.nix
      golang.go
      elixir-lsp.vscode-elixir-ls
      bbenoist.nix
      redhat.vscode-yaml
      esbenp.prettier-vscode
      # vscodevim.vim
      WakaTime.vscode-wakatime
      dracula-theme.theme-dracula
      arrterian.nix-env-selector
      scala-lang.scala
      scalameta.metals
      arrterian.nix-env-selector
      pkief.material-icon-theme
      esbenp.prettier-vscode
      # ms-vscode.cpptools
      # c-cpp-runner

      dbaeumer.vscode-eslint
      streetsidesoftware.code-spell-checker
      eamodio.gitlens
      james-yu.latex-workshop
      redhat.vscode-yaml
      foxundermoon.shell-format

      vitesse-theme
      code-stats
      jest
    ] ++ nixOsApps;

  userSettings = {
    "latex-workshop.kpsewhich.path" =
      "/etc/profiles/per-user/phv/bin/kpsewhich";
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

    "workbench.colorTheme" = "Vitesse Dark";
    # "workbench.colorTheme" = "Dracula";
    "workbench.editor.enablePreview" = false;
    "vetur.format.defaultFormatterOptions" = {
      "js-beautify-html" = { "wrap_attributes" = "aligned-multiple"; };
      "prettyhtml" = {
        "printWidth" = 400;
        "wrapAttributes" = false;
      };
    };
    "workbench.sideBar.location" = "right";
    "editor.lineNumbers" = "relative";
    "editor.fontLigatures" = true;
    "vim.handleKeys" = {
      "<C-f>" = false;
      "<C-w>" = false;
      "<C-s>" = false;
      "<C-b>" = false;
      "<C-c>" = false;
      "<C-d>" = false;
    };
    "editor.fontFamily" = "JetBrains Mono,  Iosevka, FuraCode Nerd Font";
    "editor.fontSize" = 12;
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
    "[jsonc]" = { "editor.defaultFormatter" = "esbenp.prettier-vscode"; };
    "[nix]" = { "editor.defaultFormatter" = "brettm12345.nixfmt-vscode"; };
    "[dockercompose]" = {
      "editor.defaultFormatter" = "ms-azuretools.vscode-docker";
    };
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

    "codestats.apikey" = gzpPrivateStuff.code-stat-skadi-api;
    "codestats.apiurl" = "https://codestats.net/api/";
    "codestats.username" = "phv";
  };
}
