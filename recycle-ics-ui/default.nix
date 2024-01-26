{ lib, buildNpmPackage, fetchFromGitHub, stdenv }:

# See https://zero-to-nix.com/start/nix-build
buildNpmPackage rec {
  pname = "recycle-ics-ui";
  version = "0.1.0";

  src = ./.;
  npmDepsHash = "sha256-18GoVdpMZrYvOdtTrPhSLhPEOpfMOBl40Wj8ddbIwts=";

  installPhase = ''
    mkdir -p $out
    npm run build
    cp -r build/* $out
  '';
}
