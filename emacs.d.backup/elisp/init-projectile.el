;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Projectile
(use-package projectile
  :ensure t
  :hook (after-init-hook . projectile-mode)
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package ibuffer-projectile
  :after projectile
  :ensure t)

(provide 'init-projectile)
;;; init-projectile.el ends here
