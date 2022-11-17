((nil . (
	 (haskell-mode-stylish-haskell-path . "ormolu")
	 (eval . (setq-local haskell-mode-stylish-haskell-args `("--stdin-input-file" ,buffer-file-name)))
	 (dante-methods . (new-impure-nix))
	 (dante-target . "test:recycle-test")
	 )))
