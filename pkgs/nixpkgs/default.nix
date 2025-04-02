{
  pkgs
}: let
  callPackage = pkg: pkgs.callPackage pkg;
in rec {
  when = callPackage ./when.nix {};
  kubectl-iexec = callPackage ./kubectl-iexec.nix {};

  all = [
    # when
    kubectl-iexec
  ];
}