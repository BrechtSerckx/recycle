let
  nixpkgs = import ./nix/pkgs.nix;
in { name ? "recycle", tag ? "latest" }:

let
  inherit (import ./. { release = true; }) recycle-ics;
in nixpkgs.dockerTools.buildImage {
  inherit name tag;
  fromImageName = "alpine:latest";
  copyToRoot = nixpkgs.buildEnv {
    name = "image-root";
    paths = [ recycle-ics ]
      ++ (with nixpkgs; [
        # general utils
        bash 
        coreutils
        # to fix 'certificate has unknown CA', see manual
        dockerTools.caCertificates
        # to fix 'no such protocol name: <protocol>', see manual
        iana-etc
      ]);
    pathsToLink = [ "/bin" "/etc" ];
  };
  config = {
    Entrypoint = [ "recycle-ics" ];
    Cmd = [ "-h" ];
    WorkingDir = "/srv/";
    ExposedPorts = { "3332/tcp" = { }; };
  };
}
