{ release ? false }:
let
  sources = import ./nix/sources.nix { };
  nixpkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

  # haskell projects
  haskellNix = import sources.haskellNix { };
  hsPkgs = nixpkgs.haskell-nix.project {
    src = nixpkgs.haskell-nix.haskellLib.cleanGit {
      name = "recycle";
      src = ./.;
    };
    modules = [{
      reinstallableLibGhc = true;
      packages.recycle-ics.components.exes.recycle-ics.preBuild = ''
        # link `recycle-ics-ui` to the static directory so it can be served from `recycle-ics` server
        export RECYCLE_ICS_WWW_DIR=${recycle-ics-ui}
      '';
    }] ++ (if release then [{
      packages.recycle-client.components.exes.recycle-client.dontStrip = false;
      packages.recycle-ics.components.exes.recycle-ics.dontStrip = false;
    }] else
      [ ]);
    compiler-nix-name = "ghc902";
  };

  recycle-ics-ui = nixpkgs.callPackage ./recycle-ics-ui { };
in {
  inherit sources nixpkgs hsPkgs recycle-ics-ui;

  inherit (hsPkgs.recycle-client.components.exes) recycle-client;
  inherit (hsPkgs.recycle-ics.components.exes) recycle-ics;
}
