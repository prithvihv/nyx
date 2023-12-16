{ pkgs }: {
  extraPkgs = with pkgs;
    [
      elixir_1_14
      beamPackages.rebar3
      beamPackages.hex

      asdf
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ inotify-tools ];
}
