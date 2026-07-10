{ pkgs, extraIgnores ? [ ], editor ? "vim" }:
let
  global-gitignore = pkgs.writeTextFile {
    name = ".global-gitignore";
    text = ''
      .vscode
      .project-plan
      .elixir_ls
      *.ignorefile.*
      .nvimlog
      .cursor
      .claude
    '' + pkgs.lib.concatMapStrings (x: x + "\n") extraIgnores;
    executable = false;
  };
in {
  enable = true;
  settings = {
    user = {
      name = "prithvihv";
      email = "hvprithvi09@gmail.com";
    };
    push = { autoSetupRemote = true; };
    core = {
      excludesfile = "${global-gitignore}";
      inherit editor;
    };
    # url = { "ssh://git@github" = { insteadOf = "https://github.com/"; }; };
  };
  lfs.enable = true;
}
