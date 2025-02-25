{ release ? false, build ? true }:
let
  sources = import ./nix/sources.nix { };
  nixpkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  nixpkgs-node = import sources.nixpkgs-node {};

  # haskell projects
  haskellNix = import sources.haskellNix { };
  hsPkgs = nixpkgs.haskell-nix.project {
    projectFileName = "stack.yaml";
    src = nixpkgs.haskell-nix.haskellLib.cleanGit {
      name = "recycle";
      src = ./.;
    };
    modules = [{
      reinstallableLibGhc = true;
    }] ++ (if build then [{
      # only when building, not in shell
      # fails when the recycle-ics-ui build directory doesn't exist yet
      packages.recycle-ics.components.exes.recycle-ics.preBuild = ''
        # link `recycle-ics-ui` to the static directory so it can be served from `recycle-ics` server
        export RECYCLE_ICS_WWW_DIR=${recycle-ics-ui}
      '';
    }] else []) ++ (if release then [{
      packages.recycle-ics.components.exes.recycle-ics.dontStrip = false;
    }] else
      [ ]);
  };

  recycle-ics-ui = nixpkgs-node.callPackage ./recycle-ics-ui { };
in {
  inherit sources nixpkgs hsPkgs recycle-ics-ui;

  inherit (hsPkgs.recycle-ics.components.exes) recycle-ics;
}
