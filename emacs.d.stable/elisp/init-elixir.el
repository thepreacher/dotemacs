;;; init-elixir.el --- Elixir editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-flycheck)
  (require 'init-const))

;; Elxir
;; https://github.com/elixir-lsp/elixir-ls
(use-package elixir-mode
  :after flycheck
  :mode ("\\.ex[s]\\'" . elixir-mode)
  :init
  ;; for executable of language server, if it's not symlinked on your PATH
  (add-to-list 'exec-path (expand-file-name "~/code/github/lang_servers/elixir-ls/release/")) ;; Uncomment for lsp-mode
  :hook ((elixir-mode . subword-mode)
         ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
         (elixir-mode . (lambda ()
                         (add-hook 'before-save-hook 'elixir-format nil t)))))
  ; :config
  ; (use-package flycheck-credo
  ;   :init
  ;   (setq flycheck-elixir-credo-strict t)
  ;   :config
  ;   (flycheck-credo-setup)))

(defvar lsp-elixir--config-options (make-hash-table))

(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))



(provide 'init-elixir)
;; Local Variables:
;;; init-elixir.el ends here
