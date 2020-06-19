;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

;; set shortcut to kill whole emacs session
;; needed because with emacs running as server closing a window doesnt completely
;; kill the server
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;Unbind C-z to use as a prefix
(bind-key "C-z" 'nil)

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *sys/mac* window-system)
    (suspend-frame)))

;(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking!!!\n")


(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *sys/mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


(use-package ns-auto-titlebar
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (ns-auto-titlebar-mode))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


; (add-hook 'before-make-frame-hook
;           (lambda ()
;             (add-to-list 'default-frame-alist '(left   . 744))
;             (add-to-list 'default-frame-alist '(top    . 0))
;             (add-to-list 'default-frame-alist '(height . 30))
;             (add-to-list 'default-frame-alist '(width  . 85))))

; (defun my/setup-font ()
;   (interactive)
;   (when (member "OperatorMono NF" (font-family-list))
;     (set-face-attribute 'default nil :font "OperatorMono NF-14")
;     (setq-default line-spacing 1)))

; (add-hook 'after-make-frame-functions
;           (lambda (frame)
;             (select-frame frame)
;             (my/setup-font)))



;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size easily
(use-package default-text-scale
  :ensure t
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))


(use-package disable-mouse :ensure t)


;; All The Icons
(use-package all-the-icons
  :ensure t
  :if *sys/gui*)



(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
