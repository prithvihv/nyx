{ pkgs }: {
  extraPkgs = with pkgs; [
    elixir
    elixir_ls

    beamPackages.rebar3
    beamPackages.hex
  ];
}
