;;; init-eglot.el --- Lsp client -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(advice-add 'eglot-eldoc-function :around
            (lambda (oldfun)
              (let ((help (help-at-pt-kbd-string)))
                (if help (message "%s" help) (funcall oldfun)))))

(use-package eglot
  :after flycheck
  :ensure t
  :commands (eglot eglot-ensure eglot-server-programs)
  :hook ((elixir-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (elm-mode . eglot-ensure)
         (javascript-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("/usr/local/opt/llvm/bin/clangd" "--log=error" "--background-index=false")))
  (add-to-list
    'eglot-server-programs
      '(elixir-mode . ("sh" "/Users/nahiable/Code/github/lang_servers/elixir-ls/release/language_server.sh"))))


(provide 'init-eglot)
;;; init-eglot.el ends here
