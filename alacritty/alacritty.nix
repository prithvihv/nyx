{ critty, fish  }:
let 
in
{
  # this structure belongs to the builder in home-manager!
  # https://nix-community.github.io/home-manager/options.html#opt-programs.alacritty.enable
  enable = true;
  package = critty;
  # settings = import ./alacritty.yml.nix {};
  settings = {
    colors = {
      primary = {
        background = "0x282a36";
        foreground = "0xf8f8f2";
      };
    };
  };
}
