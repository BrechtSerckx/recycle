{ release ? false }:
let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  nixpkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in nixpkgs.haskell-nix.project {
  src = nixpkgs.haskell-nix.haskellLib.cleanGit {
    name = "recycle";
    src = ./.;
  };
  modules = [{ reinstallableLibGhc = true; }] ++ (if release then [{
    packages.recycle-client.components.exes.recycle-client.dontStrip = false;
  }] else
    [ ]);
  compiler-nix-name = "ghc902";
}
