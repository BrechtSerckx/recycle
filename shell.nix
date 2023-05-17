let
  inherit (import ./default.nix {}) sources nixpkgs hsPkgs;
  nixpkgs-node = import sources.nixpkgs-node {};
  nixpkgs-act = import sources.nixpkgs-act {};
in hsPkgs.shellFor {
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
    ]) ++ (with nixpkgs-act;  [
    # run ci/cd locally
    act
    ]) ++ (with nixpkgs-node;  [
    # nodejs for frontend
    nodejs
    prefetch-npm-deps
    ]) ++ (with nixpkgs-node.nodePackages; [
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
