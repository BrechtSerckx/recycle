{ lib, buildNpmPackage, fetchFromGitHub, stdenv }:

buildNpmPackage rec {
  pname = "recycle-ics-ui";
  version = "0.1.0";
  src = ./.;
  npmDepsHash = "sha256-18GoVdpMZrYvOdtTrPhSLhPEOpfMOBl40Wj8ddbIwts=";
}
