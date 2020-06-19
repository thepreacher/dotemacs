(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode))
  :hook ((haskell-mode . lsp)
         (haskell-mode . turn-on-haskell-indentation))
  :init
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  :config
  (use-package intero
    :ensure t
    :hook ((haskell-mode . subword-mode)
           (haskell-mode . eldoc-mode))
    :bind (:map intero-mode-map
                ("M-?" . nil))
    :config
    (intero-global-mode)
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))


(provide 'init-haskell)
