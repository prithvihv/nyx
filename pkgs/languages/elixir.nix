{ pkgs }: {
  extraPkgs = with pkgs;
    [
      beam.packages.erlang_28.elixir
      beamPackages.expert
      beamPackages.erlang
      beamPackages.rebar3
      beamPackages.hex
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ 
      inotify-tools

      livebook
    ];
}
