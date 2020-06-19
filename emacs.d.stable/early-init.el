;;; early-init.el --- Emacs 27+ pre-initialisation config
;;  Author: Napoleon Ahiable
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)


;; Ref: https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(add-to-list 'initial-frame-alist '(width  . 87))
(add-to-list 'initial-frame-alist '(height . 75))
(add-to-list 'default-frame-alist '(font . "OperatorMono NF-14"))

;; So we can detect this having been loaded
(provide 'early-init)
;; Local Variables:
;;; early-init.el ends here
