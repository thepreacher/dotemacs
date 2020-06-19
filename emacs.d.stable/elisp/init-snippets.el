;;; init-snippets.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize snippets
;;
;;; Commentary:
;;
;;; Code:
(use-package yasnippet
  :hook ((
           python-mode
           js2-mode
           web-mode
           elixir-mode
           elm-mode
           rust-mode
           ruby-mode
           scala-mode
           go-mode
           haskell-mode) . yas-minor-mode)
  :preface
  (defvar tmp/company-point nil)
  :config
  (use-package yasnippet-snippets)
  (advice-add 'company-complete-common
              :before
              #'(lambda ()
                  (setq tmp/company-point (point))))
  (advice-add 'company-complete-common
              :after
              #'(lambda ()
                  (when (equal tmp/company-point (point))
                    (yas-expand)))))



(provide 'init-snippets)
;; Local Variables:
;;; init-snippets.el ends here
