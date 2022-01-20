{ pkgs }: {
  extraPkgs = with pkgs; [
    # rustc
    # rustup
    # rustup-toolchain-install-master

    # binutils.bintools
    # rustfmt
    # rust-analyzer

    # cargo
  ];
}
