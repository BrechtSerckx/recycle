let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  nixpkgs = haskellNix.pkgs-unstable;
in { name ? "recycle", tag ? "latest" }:

let recycle = (import ./. { release = true; }).recycle.components.exes.recycle;
in nixpkgs.dockerTools.buildImage {
  inherit name tag;
  fromImageName = "alpine:latest";
  copyToRoot = nixpkgs.buildEnv {
    name = "image-root";
    paths = [ recycle ] ++ (with nixpkgs; [ coreutils cacert ]);
    pathsToLink = [ "/bin" ];
  };
  config = {
    Entrypoint = [ "recycle" ];
    Cmd = [ "-h" ];
    WorkingDir = "/app/";
    ExposedPorts = { "3332/tcp" = { }; };
  };
}
