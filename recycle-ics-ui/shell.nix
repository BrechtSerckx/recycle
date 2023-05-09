let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in nixpkgs.mkShell {
  buildInputs = (with nixpkgs; [ nixfmt ]) ++ (with nixpkgs; [ nodejs ])
    ++ (with nixpkgs.nodePackages; [ npm ]);
}
