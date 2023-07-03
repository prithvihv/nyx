{ pkgs }: {
  extraPkgs = with pkgs;
    [
      elixir
      elixir_ls

      beamPackages.rebar3
      beamPackages.hex

      asdf
    ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ inotify-tools ];
}
