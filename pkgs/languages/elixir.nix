{ pkgs }: {
  extraPkgs = with pkgs;
    [
      elixir
      beamPackages.rebar3
      beamPackages.hex

      asdf
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ 
      inotify-tools

      livebook
    ];
}
