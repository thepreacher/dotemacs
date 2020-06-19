;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package js2-mode
  :preface
  (defun srs|setup-tide-js ()
    "Setup tide for javascript."
    (interactive)
    (tide-setup)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :hook (js2-mode . srs|setup-tide-js)
  :config
  ;; js2-refactor
  (use-package js2-refactor
    :hook (js2-mode . js2-refactor-mode)
    :config
    (js2r-add-keybindings-with-prefix "C-c C-r"))

  ;; Better javascript jump to definition
  (use-package xref-js2
    :preface
    (defun srs|setup-xref-js2 ()
      "Setup xref js2"
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
    :hook (js2-mode . srs|setup-xref-js2)))

(use-package indium
  :hook ((js2-mode typescript-mode) . indium-interaction-mode))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))


;; Typescript
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :preface
  (defun srs|setup-tide-ts ()
    "Setup tide for typescript."
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode))
  :hook (typescript-mode . srs|setup-tide-ts))

(use-package tide
  :defer t
  :commands (tide-setup)
  :after (typescript-mode company flycheck)
  :preface
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
