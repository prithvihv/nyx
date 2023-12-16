{ config, pkgs, lib, ... }:

# Used to import optionals function in scope
with lib;

let builds = import ./builds.nix;
in {
  # Option declaration
  options.nixos-system-build = mkOption {
    description =
      "defines nixos builds context, can be used to if-else cases matching based on build";

    type = types.enum (attrValues builds);
  };
}
