;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Dracula
(use-package dracula-theme
  :config
  (load-theme 'dracula t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))


;; Atom One Dark
; (use-package atom-one-dark-theme
;   :config
;   (load-theme 'atom-one-dark t)
;   (let ((line (face-attribute 'mode-line :underline)))
;     (set-face-attribute 'mode-line          nil :overline   line)
;     (set-face-attribute 'mode-line-inactive nil :overline   line)
;     (set-face-attribute 'mode-line-inactive nil :underline  line)
;     (set-face-attribute 'mode-line          nil :box        nil)
;     (set-face-attribute 'mode-line-inactive nil :box        nil)
;     (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; Sanityinc Tomorrow (Dark)
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t)
;;   (let ((line (face-attribute 'mode-line :underline)))
;;     (set-face-attribute 'mode-line          nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :underline  line)
;;     (set-face-attribute 'mode-line          nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))


(use-package ns-auto-titlebar
  :if *sys/mac*
  :config
  (ns-auto-titlebar-mode))

(provide 'init-themes)
;;; init-themes.el ends here
