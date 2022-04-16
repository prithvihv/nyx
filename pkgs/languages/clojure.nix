{ pkgs }: {
  extraPkgs = with pkgs; [
    clojure
    leiningen


    clojure-lsp
  ];
}
