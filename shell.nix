let
  inherit (import ./default.nix { build = false; }) sources nixpkgs hsPkgs;
  nixpkgs-node = import sources.nixpkgs-node {};
  nixpkgs-act = import sources.nixpkgs-act {};
  # built via its own pinned nixpkgs (npins), kept out of the recycle-ics cabal deps
  mcp-hoogle = import sources.mcp-hoogle { };
in hsPkgs.shellFor {
  packages = ps: with ps; [
    recycle-ics
  ];

  withHoogle = true;

  tools = {
    cabal = { version = "latest"; };
    haskell-language-server = "latest";
    ghcid = { version = "latest"; };
    hlint = { version = "latest"; };
    ormolu = { version = "latest"; };
    cabal-fmt = "latest";
  };

  buildInputs = (with nixpkgs; [
    niv
    (import sources.niv { }).niv
    # task runner
    just
    # nix formatter
    nixfmt
    # haskell ci/cd generator
    haskell-ci
    ]) ++ [
    # MCP server exposing hoogle search over local haskell deps
    mcp-hoogle
    ] ++ (with nixpkgs-act;  [
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
  shellHook = ''
    export PATH="$PWD/recycle-ics-ui/node_modules/.bin/:$PATH"
  '';

  exactDeps = true;
}
