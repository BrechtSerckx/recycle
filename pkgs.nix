let
  sources = import ./nix/sources.nix;
  overlays = [ (import ./overlay.nix) ];
in import sources.nixpkgs { inherit overlays; }
