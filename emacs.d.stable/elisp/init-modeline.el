;;; init-modeline.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Constants
;; Compatibility: emacs-version >= 26.1
;;
;;; Commentary:
;; Modeline styling and appearance
;;; Code:
;; A minor-mode menu for the mode line
(use-package minions
  :hook (after-init . minions-mode)
  :config
  (setq
    minions-mode-line-lighter "[+]"
    minions-direct '(flycheck-mode pyvenv-mode))
  (when (member "OperatorMono NF" (font-family-list))
    (let ((faces '(mode-line
                mode-line-buffer-id
                mode-line-emphasis
                mode-line-highlight
                mode-line-inactive)))
      (mapc
        (lambda (face) (set-face-attribute face nil :foreground "white" :font "OperatorMono NF-14"))
        faces))))

;; Tabs and ribbons for the mode line
(use-package moody
  :config
  (setq moody-slant-function #'moody-slant-apple-rgb)  ;; Not required when runing emacs-mac
  (setq x-underline-at-descent-line t
        moody-mode-line-height 25)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :defer t
  :init
  (add-hook 'after-init-hook 'mode-line-bell-mode))

(provide 'init-modeline)
;; Local Variables:
;;; init-modeline.el ends here
