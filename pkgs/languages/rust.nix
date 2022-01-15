{ pkgs }: {
  extraPkgs = with pkgs; [
    rustc
    rustfmt
    rust-analyzer

    cargo
  ];
}
