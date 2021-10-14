let pkgs = import ./pkgs.nix;
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.recycle ];
  buildInputs = with pkgs; [
    haskellPackages.cabal-install
    haskellPackages.brittany
    haskellPackages.ghcid
    haskellPackages.hlint
  ];
}
