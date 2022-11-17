let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  nixpkgs = haskellNix.pkgs-unstable;
in (import ./default.nix).shellFor {
  packages = ps: with ps; [ recycle ];

  withHoogle = true;

  tools = {
    cabal = { version = "latest"; };
    ghcid = { version = "latest"; };
    hlint = { version = "latest"; };
  };

  buildInputs = (with nixpkgs; [
    (import sources.niv { }).niv
    nixfmt
    haskellPackages.brittany
  ]) ++ (with nixpkgs.nodePackages; [ js-beautify eslint ]);
}
