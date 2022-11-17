let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "recycle";
    src = ./.;
  };
  modules = [{
    reinstallableLibGhc = true;
  }];
  compiler-nix-name = "ghc925";
}
