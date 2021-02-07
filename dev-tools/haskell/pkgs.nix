{pkgs}:
with pkgs;
rec{
   programs = [
     ghc
     ghcid
     cabal-install
   ];
}
