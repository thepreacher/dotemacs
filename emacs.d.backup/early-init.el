;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; Set default window startup size and position
(unless (display-graphic-p)
    (progn
      (setq initial-frame-alist '((top . 0) (left . 744) (width . 85) (height . 30)))
      (setq default-frame-alist '((top . 0) (left . 744) (width . 85) (height . 30)))))
;; -Set default window startup size and position

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; -DisableUnnecessaryInterface


;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.
(if (eq system-type 'darwin)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame)
                                         1 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))


;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
