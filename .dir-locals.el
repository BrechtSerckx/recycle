(
 (haskell-mode . (
                  ;; Lsp
                  (haskell-completion-backend . lsp)
                  (lsp-haskell-server-path . "haskell-language-server")
                  ;; Ormolu as formatter
                  (lsp-haskell-formatting-provider . "ormolu")
                  (haskell-mode-stylish-haskell-path . "ormolu")
                  ;; Use buffer name as ormolu file name
                  (eval . (setq haskell-mode-stylish-haskell-args `("--stdin-input-file" ,buffer-file-name) ))
                  ;; Format on save
                  (haskell-stylish-on-save . t)
                  ))
 )
