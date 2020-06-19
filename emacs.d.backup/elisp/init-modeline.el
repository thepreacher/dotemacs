
;; A minor-mode menu for the mode line
(use-package minions
  :ensure t
  :config
  (setq
    minions-mode-line-lighter "[+]"
    minions-direct '(flycheck-mode pyvenv-mode))
  (minions-mode 1))

;; Tabs and ribbons for the mode line
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t
        moody-mode-line-height 25
        moody-slant-function #'moody-slant-apple-rgb)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

; (use-package doom-modeline
;   :ensure t
;   :hook (after-init . doom-modeline-mode)
;   :config
;   (setq doom-modeline-project-detection 'projectile)
;   (setq doom-modeline-minor-modes t)
;   (setq doom-modeline-github t))



(provide 'init-modeline)
