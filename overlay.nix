self: super:
let
  hlib = super.haskell.lib;
  lib = super.lib;
  sources = import nix/sources.nix;
  gitignore = path:
    super.nix-gitignore.gitignoreSourcePure [ (path + /.gitignore) ] path;
  overrides = selfh: superh: {
    recycle = superh.callCabal2nix "recycle" (gitignore ./.) {};
    deriving-aeson = superh.callCabal2nix "deriving-aeson" sources.deriving-aeson {};
    servant = superh.callCabal2nixWithOptions "servant" sources.servant "--subpath servant" {};
    servant-client-core = superh.callCabal2nixWithOptions "servant" sources.servant "--subpath servant-client-core" {};
    servant-client = superh.callCabal2nixWithOptions "servant" sources.servant "--subpath servant-client" {};
    servant-server = hlib.dontCheck(superh.callCabal2nixWithOptions "servant" sources.servant "--subpath servant-server" {});
    typerep-map = hlib.dontCheck(superh.typerep-map);
    iCalendar = superh.callCabal2nix "iCalendar" sources.iCalendar {};
  };
in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) overrides;
  });
}
