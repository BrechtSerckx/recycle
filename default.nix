{ release ? false }:
let
  nixpkgs = import ./nix/pkgs.nix;
in nixpkgs.haskell-nix.project {
  src = nixpkgs.haskell-nix.haskellLib.cleanGit {
    name = "recycle";
    src = ./.;
  };
  modules = [{ reinstallableLibGhc = true; }] ++ (if release then [{
    packages.recycle-client.components.exes.recycle-client.dontStrip = false;
    packages.recycle-ics.components.exes.recycle-ics.dontStrip = false;
  }] else
    [ ]);
  compiler-nix-name = "ghc902";
}
