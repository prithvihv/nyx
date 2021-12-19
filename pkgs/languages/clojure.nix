{ pkgs }: {
  extraPkgs = with pkgs; [
    clojure
    leiningen
  ];
}
