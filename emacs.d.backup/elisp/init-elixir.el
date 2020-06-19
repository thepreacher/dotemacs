;;; init-elixir.el --- Elixir editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elxir
;; https://github.com/elixir-lsp/elixir-ls
(use-package elixir-mode
  :ensure t
  :after (company)
  :mode ("\\.ex[s]\\'" . elixir-mode)
  :hook (
         (elixir-mode . subword-mode)
    ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
         (elixir-mode . (lambda ()
                         (add-hook 'before-save-hook 'elixir-format nil t))))
  :init
  (add-to-list 'exec-path (expand-file-name "~/Code/github/lang_servers/elixir-ls/release")) ;; Uncomment for lsp-mode
  :config
  (use-package flycheck-mix
    :ensure t
    :config
    (eval-after-load 'flycheck
      '(flycheck-mix-setup))))


(provide 'init-elixir)
;;; init-elixir.el ends here
