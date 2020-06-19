;;; init-elm.el --- Support for the Elm language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :after (company)
  :config
  (when (executable-find "elm-format")
    (setq-default elm-format-on-save t))
  (push 'company-elm company-backends)

  (use-package elm-test-runner)

  (use-package flycheck-elm
    :hook (flycheck-mode . flycheck-elm-setup)))



(provide 'init-elm)
;; Local Variables:
;;; init-elm.el ends here
