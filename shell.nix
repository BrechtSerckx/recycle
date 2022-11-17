let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  nixpkgs = haskellNix.pkgs-unstable;
in (import ./default.nix {}).shellFor {
  packages = ps: with ps; [ recycle ];

  withHoogle = true;

  tools = {
    cabal = { version = "latest"; };
    ghcid = { version = "latest"; };
    hlint = { version = "latest"; };
  };

  buildInputs = (with nixpkgs; [
    # niv
    (import sources.niv { }).niv
    # nix formatter
    nixfmt
    # haskell formatter (not using haskell.nix, unfortunately)
    haskellPackages.brittany
    # haskell ci/cd generator
    haskell-ci
  ]) ++ (with nixpkgs.nodePackages; [
    # web formatter
    js-beautify
    # js linter
    eslint
  ]);
}
