{ pkgs }: {
  extraPkgs = with pkgs; [
    elixir
    elixir_ls
    inotify-tools

    beamPackages.rebar3
    beamPackages.hex

    asdf
  ];
}
