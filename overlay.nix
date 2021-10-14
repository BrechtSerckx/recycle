self: super:
let
  hlib = super.haskell.lib;
  lib = super.lib;
  sources = import nix/sources.nix;
  gitignore = path:
    super.nix-gitignore.gitignoreSourcePure [ (path + /.gitignore) ] path;
  overrides = selfh: superh: {
    recycle = superh.callCabal2nix "recycle" (gitignore ./.) {};
  };
in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) overrides;
  });
}
