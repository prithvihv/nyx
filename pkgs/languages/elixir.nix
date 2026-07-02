{ pkgs }: {
  extraPkgs = with pkgs;
    [
      elixir_1_18
      beamPackages.expert
      beamPackages.erlang
      beamPackages.rebar3
      beamPackages.hex
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ 
      inotify-tools

      livebook
    ];
}
