{ pkgs }:
let
  global-gitignore = pkgs.writeTextFile {
    name = ".global-gitignore";
    text = ''
      .vscode
      *.ignorefile.*
    '';
  };
in {
  enable = true;
  userName = "prithvihv";
  userEmail = "hvprithvi09@gmail.com";
  extraConfig = {
    push = { autoSetupRemote = true; };
    core = { excludesfile = "${global-gitignore}"; };
  };
}
