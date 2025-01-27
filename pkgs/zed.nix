{ pkgs }:
let 
  # priv = import ../priv/priv.nix { };
in {
    enable = true;
    # https://github.com/zed-industries/extensions/tree/main/extensions
    extensions = ["nix" "make" "sql" "docker-compose" "dockerfile" "helm"];

}