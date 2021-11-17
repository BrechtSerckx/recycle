let sources = import ./nix/sources.nix;
in { pkgs ? import sources.nixpkgs { }, name ? "recycle", tag ? "latest" }:

let recycle = pkgs.haskell.lib.justStaticExecutables (import ./.);
in pkgs.dockerTools.buildImage {
  inherit name tag;
  fromImageName = "alpine:latest";
  contents = [ recycle ] ++ (with pkgs; [ coreutils cacert ]);
  config = {
    Entrypoint = [ "recycle" ];
    Cmd = [ "-h" ];
    WorkingDir = "/app/";
    ExposedPorts = {
      "3332/tcp" = { };
    };
  };
}
