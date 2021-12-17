{ pkgs }: {
  enable = true;
  functions = {
    gitignore = "curl -sL https://www.gitignore.io/api/$argv";
  };

  plugins = [
    {
      name = "theme-batman";
      src = pkgs.fetchFromGitHub {
        owner = "oh-my-fish";
        repo = "theme-batman";
        rev = "2a76bd81f4805debd7f137cb98828bff34570562";
        sha256 = "Ko4w9tMnIi17db174FzW44LgUdui/bUzPFEHEHv//t4=";
      };
    }
  ];

  shellAbbrs = { 
    gco = "git checkout"; 
    gc = "git commit";
    gl = "git log";
  };

  shellAliases = {
      "..." = "cd ../..";
      "db_local" = "psql -U postgres";
      "db_dev" = "echo 'todo'";
      "cd_hdd_sw" = "cd /run/media/phv/bf8da584-7bf2-425c-a602-8b3b997814d0";
      "cd_hdd_data" = "cd /run/media/phv/d930b0b1-853e-45d7-b249-f71f7108b3ac";
      "cd_gz" = "cd /home/phv/code/src/github.com/gamezop";
      "j" = "ssh gz_jump";
  };
}
