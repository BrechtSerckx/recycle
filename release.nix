let pkgs = import ./pkgs.nix; in { inherit (pkgs.haskellPackages) recycle; }

