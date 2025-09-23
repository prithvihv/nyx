{ pkgs }: {
  extraPkgs = with pkgs;
    [
      elixir_1_16
      beamPackages.erlang
      beamPackages.rebar3
      beamPackages.hex

      asdf
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ 
      inotify-tools

      livebook
    ];
}
