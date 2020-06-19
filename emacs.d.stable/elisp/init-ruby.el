;;; init-ruby.el --- ruby editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elxir
(use-package ruby-mode
  :after (company)
  :mode (("Rakefile\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.rxml\\'" . ruby-mode)
         ("\\.rjs\\'" . ruby-mode)
         ("\\.irbrc\\'" . ruby-mode)
         ("\\.pryrc\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Kirkfile\\'" . ruby-mode))
  :hook ((ruby-mode . subword-mode)
         (ruby-mode . lsp))
  :init
  (setq-default
    ruby-use-encoding-map nil
    ruby-insert-encoding-magic-comment nil)
  :config
  ;; Create a buffer-local hook to run format on save, only when we enable ruby-mode.
  (add-hook 'ruby-mode-hook
            (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t))))


(provide 'init-ruby)
;;; init-ruby.el ends here
