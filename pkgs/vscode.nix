{ pkgs, gzpPrivateStuff }: {
  enable = true;
  # package = pkgs.vscodium;
  extensions = with pkgs.vscode-extensions; [
    # language
    bbenoist.nix
    golang.go
    elixir-lsp.vscode-elixir-ls
    redhat.vscode-yaml
    esbenp.prettier-vscode
    # vscodevim.vim
    WakaTime.vscode-wakatime
    dracula-theme.theme-dracula
    arrterian.nix-env-selector
    scala-lang.scala
    scalameta.metals
  ];

  userSettings = {
    "go.coverOnSave" = true;
    "go.coverageDecorator" = {
      "type" = "gutter";
      "coveredHighlightColor" = "rgba(64,128,128,0.5)";
      "uncoveredHighlightColor" = "rgba(128,64,64,0.25)";
      "coveredGutterStyle" = "blockgreen";
      "uncoveredGutterStyle" = "blockred";
    };
    "go.coverOnSingleTest" = true;

    "workbench.colorTheme" = "Dracula";
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
    "[clojure]" = {
      "editor.autoClosingBrackets" = "always";
      "editor.autoClosingOvertype" = "always";
      "editor.formatOnPaste" = true;
    };
    "[nix]" = { "editor.defaultFormatter" = "brettm12345.nixfmt-vscode"; };
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
    ];
    "aws.profile" = "profile=default";
    "window.zoomLevel" = 1;

    "calva.clojureLspPath" = "/etc/profiles/per-user/phv/bin/clojure-lsp";

    "codestats.apikey" = gzpPrivateStuff.code-stat-skadi-api;
    "codestats.apiurl" = "https://codestats.net/api/";
    "codestats.username" = "phv";
  };
}
