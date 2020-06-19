;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :config
  (diredfl-global-mode)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(use-package diff-hl
  :hook (dired-mode-hook . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))

(use-package all-the-icons-dired
  :if *sys/gui*
  :delight
  :custom-face
  (all-the-icons-dired-dir-face ((t `(:foreground ,(face-background 'default)))))
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; Workaround for all-the-icons bug until PR merged https://github.com/domtronn/all-the-icons.el/pull/150
  (when (require 'all-the-icons nil 'noerror)
    (setq all-the-icons-mode-icon-alist
          (delete '(erc-mode all-the-icons-faicon "commenting-o" :height 1.0 :v-adjust 0.0 :face all-the-icons-white) all-the-icons-mode-icon-alist))
    (add-to-list 'all-the-icons-mode-icon-alist '(erc-mode all-the-icons-faicon "commenting-o" :height 1.0 :v-adjust 0.0))))

(provide 'init-dired)
;;; init-dired.el ends here
