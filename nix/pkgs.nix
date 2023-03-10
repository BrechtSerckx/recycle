let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
  nixpkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in nixpkgs
