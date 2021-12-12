{ pkgs }: {
  enable = true;
  functions = {
    gitignore = "curl -sL https://www.gitignore.io/api/$argv";
  };

  plugins = [{
    name = "fasd";
    src = pkgs.fetchFromGitHub {
      owner = "oh-my-fish";
      repo = "plugin-fasd";
      rev = "38a5b6b6011106092009549e52249c6d6f501fba";
      sha256 = "06v37hqy5yrv5a6ssd1p3cjd9y3hnp19d3ab7dag56fs1qmgyhbs";
    };
  }];

  shellAbbrs = { gco = "git checkout"; };

  shellAliases = {
      "..." = "cd ../..";
      "cd_hdd_sw" = "cd /run/media/phv/bf8da584-7bf2-425c-a602-8b3b997814d0";
      "cd_hdd_data" = "cd /run/media/phv/d930b0b1-853e-45d7-b249-f71f7108b3ac";
      "cd_gz" = "cd /home/phv/code/src/github.com/gamezop";
  };
}
