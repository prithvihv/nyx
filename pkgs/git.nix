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
    '';
    executable = false;
  };
in {
  enable = true;
  userName = "prithvihv";
  userEmail = "hvprithvi09@gmail.com";
  extraConfig = {
    push = { autoSetupRemote = true; };
    core = {
      excludesfile = "${global-gitignore}";
      editor = "vim";
    };
    url = { "ssh://git@github" = { insteadOf = "https://github.com/"; }; };
  };
  lfs.enable = true;
}
