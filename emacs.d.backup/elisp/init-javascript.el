;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package js2-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :init
  (defun srs|setup-tide-js ()
    "Setup tide for javascript."
    (interactive)
    (tide-setup)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))
  (add-hook 'js2-mode-hook #'srs|setup-tide-js)

  :config
  ;; New line between curly brackets and parens
  (with-eval-after-load "smartparens"
    (sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
    (sp-local-pair 'js2-mode "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

  ;; js2-refactor
  (use-package js2-refactor
    :ensure t
    :defer t
    :hook (js2-mode . js2-refactor-mode)
    :config
    (js2r-add-keybindings-with-prefix "C-c C-r"))

  ;; Better javascript jump to definition
  (use-package xref-js2
    :ensure t
    :config
    (defun srs|setup-xref-js2 ()
      "Setup xref js2"
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
    (add-hook 'js2-mode-hook #'srs|setup-xref-js2)))

(use-package indium
  :ensure t
  :defer t
  :hook ((js2-mode typescript-mode) . indium-interaction-mode))

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'" . json-mode)
  :config
  ;; New line between curly brackets and parens
  (sp-local-pair 'json-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'json-mode "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))


;; Typescript
(use-package typescript-mode
  :ensure t
  :defer t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (defun srs|setup-tide-ts ()
    "Setup tide for typescript."
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode))
  (add-hook 'typescript-mode-hook #'srs|setup-tide-ts)
  :config
  ;; New line between curly brackets and parens
  (sp-local-pair 'typescript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'typescript-mode "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(use-package tide
  :ensure t
  :defer t
  :commands (tide-setup)
  :after (typescript-mode company flycheck)
  :config
  (defun srs/create-jsconfig ()
    "Create jsconfig.json in directory of current buffer and restarts tide server"
    (interactive)
    (copy-file (concat user-emacs-directory "misc/jsconfig.json") (concat (file-name-directory buffer-file-name) "jsconfig.json"))
    (tide-restart-server))

  (defun srs/create-tsconfig ()
    "Create tsconfig.json in directory of current buffer and restarts tide server"
    (interactive)
    (copy-file (concat user-emacs-directory "misc/tsconfig.json") (concat (file-name-directory buffer-file-name) "tsconfig.json"))
    (tide-restart-server)))



(provide 'init-javascript)
;;; init-js.el ends here
