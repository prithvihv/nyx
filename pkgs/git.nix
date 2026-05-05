{ pkgs }:
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
      AGENTS.md
      .claude
      CLAUDE.md
    '';
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
      editor = "vim";
    };
    # url = { "ssh://git@github" = { insteadOf = "https://github.com/"; }; };
  };
  lfs.enable = true;
}
