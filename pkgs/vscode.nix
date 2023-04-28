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
          version = "0.6.4";
          sha256 = "sha256-6nIzHJsLsIG3O6p97Q+YdDKxHj7r+pEwiq0UbJ/vlf4=";
        };
        meta = with pkgs.lib; {
          description = "Vitesse theme for VS Code";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=antfu.theme-vitesse";
          homepage = "no";
          license = licenses.mit;
        };
      };
      code-spell-checker = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
        mktplcRef = {
          name = "code-spell-checker";
          publisher = "streetsidesoftware";
          version = "2.20.4";
          sha256 = "sha256-GOXKXZPEynyqRUUY0pdNwt+141kJleg74IbCP4/34R8=";
        };
        meta = with pkgs.lib; {
          description =
            "A basic spell checker that works well with code and documents.";
          downloadPage =
            "https://marketplace.visualstudio.com/items?itemName=streetsidesoftware.code-spell-checker";
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

    in [
      vscode-jest
      vscode-test-explorer
      bbenoist.nix
      brettm12345.nixfmt-vscode
      code-spell-checker
      eamodio.gitlens
      golang.go
      elixir-lsp.vscode-elixir-ls
      prisma.prisma
      redhat.vscode-yaml
      esbenp.prettier-vscode
      # vscodevim.vim
      WakaTime.vscode-wakatime
      dracula-theme.theme-dracula
      vitesse-theme
      arrterian.nix-env-selector
      scala-lang.scala
      scalameta.metals
      ms-vsliveshare.vsliveshare
      ms-vscode-remote.remote-ssh
      redhat.java

      dbaeumer.vscode-eslint
    ];

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
    "java.jdt.ls.java.home" = "${pkgs.openjdk17}/lib/openjdk";

    "codestats.apikey" = gzpPrivateStuff.code-stat-skadi-api;
    "codestats.apiurl" = "https://codestats.net/api/";
    "codestats.username" = "phv";

    "java.configuration.runtimes" = [{
      name = "JavaSE-17";
      path = "${pkgs.openjdk17}/lib/openjdk";
      default = true;
    }];
  };
}
