{ pkgs }:
let 
    nixExt = (with pkgs.vscode-extensions; [
        bbenoist.Nix
    ]);

    dataFileFormatsExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "better-toml";
            publisher = "bungcip";
            version = "0.3.2";
            sha256 = "83e2df8230274ae4a3fe74a694f741d2ddbd92a4e67a0641e41d5c6333fc9022";
        }
    ]);

    dockerExt = (with pkgs.vscode-extensions;[
       ms-azuretools.vscode-docker 
    ]);

    themeExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "theme-dracula";
            publisher = "dracula-theme";
            version = "2.22.3";
            sha256 = "869f334c553ae9bb7f64af1aed599019753729325a789a5164a4d818b34ed172";
	    }    
    ]);

    elixirExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "elixir-ls";
            publisher = "jakebecker";
            version = "0.6.5";
            sha256 = "e0ba6589639db8c9e96caddb714ac741f5594bcfd49b308c0e5625a83c80f8c8"; 
        }
    ]);

    erlangExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "erlang";
            publisher = "pgourlain";
            version = "0.6.9";
            sha256 = "6681ba74a65c0463ae8bb2d3105812fe432533b8e795688476a713e4f1766f7d"; 
        }
    ]);

    haskellExt = (with pkgs.vscode-extensions;[
        haskell.haskell
    ]) ++ (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "language-haskell";
            publisher = "justusadam";
            version = "3.3.0";
            sha256 = "dab96899ce2a8dc6b532fe65c604ffe00011cee1bc7b8920b2189e96905e0589";
        }
    ]);

    elmExt = (with pkgs.vscode-extensions;[
	    elmtooling.elm-ls-vscode
    ]);
    
    fishExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "vscode-fish";
            publisher = "bmalehorn";
            version = "1.0.20";
            sha256 = "684e312fdb4f8f55360768be98c58a36049a856a41db8faf1d85eb53dc11071a"; 
        }
    ]);


    GolangExt = (with pkgs.vscode-extensions; [
	    golang.Go
    ]);

    protoExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "vscode-protolint";
            publisher = "plex";
            version = "0.5.1";
            sha256 = "687e9efd2e7b3b751c5b5b988ebaa9dfefac6e8a7298677f252df5c7930abb31";
	    }
        {
            name = "vscode-proto3";
            publisher = "zxh404";
            version = "0.5.3";
            sha256 = "a144a287e61d0176244cd7a36412637177b041ec2040530684015d27f08131de";
        }
    ]);

    gitExt = (pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
            name = "gitlens";
            publisher = "eamodio";
            version = "11.2.1";
            sha256 = "6613130a1968a14d4eb0edb83965db529a8b7ad8ab534c7f072ced7ab21647ad";
        }
    ]);

    misc = (with pkgs.vscode-extensions; [
       WakaTime.vscode-wakatime
       # needs node to be installed, programs attribute in the resulting attr has it
       # need to change .vscode-server/xxxxx/bin/server.sh to use global node instead of local
       ms-vscode-remote.remote-ssh
    ]);

    allExtensions = 
       nixExt 
	    ++ misc
        ++ dataFileFormatsExt
        ++ protoExt
        ++ themeExt
        ++ gitExt
        ++ dockerExt
        ++ fishExt
        ++ GolangExt
        ++ elixirExt
        ++ erlangExt
        ++ haskellExt;
in {
    config = {
      enable = true;
      package = pkgs.vscodium;
      extensions = allExtensions;
    };

    programs = with pkgs;[
    	nodejs-12_x
    ];
}
