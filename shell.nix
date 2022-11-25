let
  sources = import ./nix/sources.nix { };
  nixpkgs = import ./nix/pkgs.nix;
in (import ./default.nix {}).shellFor {
  packages = ps: with ps; [ recycle-client recycle-ics ];

  withHoogle = true;

  tools = {
    cabal = { version = "latest"; };
    ghcid = { version = "latest"; };
    hlint = { version = "latest"; };
    # 2023-05-09: "latest" did not work
    ormolu = { version = "0.5.1.0"; };
  };

  buildInputs = (with nixpkgs; [
    # niv
    (import sources.niv { }).niv
    # nix formatter
    nixfmt
    # haskell ci/cd generator
    haskell-ci
    # nodejs for frontend
    nodejs
  ]) ++ (with nixpkgs.nodePackages; [
    # create-react-app
    create-react-app
    # package manager
    npm
    # web formatter
    prettier
    js-beautify
    # js linter
    eslint
  ]);
}
