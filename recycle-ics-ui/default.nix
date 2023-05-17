{ lib, buildNpmPackage, fetchFromGitHub, stdenv }:

stdenv.mkDerivation {
  pname = "recycle-ics-ui";
  version = "0.1.0";
  src = ./build;
  buildPhase = ''
    mkdir -p $out
    cp -r $src/* $out/
  '';
}
# buildNpmPackage rec {
#   pname = "recycle-ics-ui";
#   version = "0.1.0";

#   src = ./.;

#   npmDepsHash = "sha256-18GoVdpMZrYvOdtTrPhSLhPEOpfMOBl40Wj8ddbIwts=";

#   # The prepack script runs the build script, which we'd rather do in the build phase.
#   # makeCacheWritable = true;
#   # npmPackFlags = [ "--ignore-scripts" ];
#   # npmFlags = [ "--legacy-peer-deps" ];
#   # NODE_OPTIONS = "--openssl-legacy-provider";
# }
