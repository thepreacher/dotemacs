;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;; Reference .emacs.d's are:
;; https://github.com/purcell/emacs.d
;; https://github.com/poncie/.emacs.d
;; https://github.com/MatthewZMD/.emacs.d

;;; Code:

;; Produce backtraces when errors occur
;(setq debug-on-error t)

;; CheckVer
(cond ((version< emacs-version "26.1")
       (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))
;; -CheckVer

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
;; BetterGC
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; (add-hook 'emacs-startup-hook
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq message-log-max 10000)

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

;;(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(defconst my-savefile-dir (expand-file-name "savefile" user-emacs-directory))


;; create the savefile dir if it doesn't exist
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

;; Constants
(require 'init-const)

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
;;(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH
(require 'init-paradox) ;; Improve default package list menu

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-frame-hooks)
(require 'init-gui-frames)

;;----------------------------------------------------------------------------
;; Load UI editing configs
;;----------------------------------------------------------------------------
(require 'init-fonts)
(require 'init-themes)
(require 'init-modeline)
;(require 'init-dashboard)

;;----------------------------------------------------------------------------
;; Load text editing configs
;;----------------------------------------------------------------------------
(require 'init-macos)
(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-smartparens)
(require 'init-spelling)
(require 'init-yasnippet)
(require 'init-flycheck)
;(require 'init-flymake)
(require 'init-ivy)
(require 'init-company)
(require 'init-projectile)
(require 'init-treemacs)
(require 'init-git-magit)
(require 'init-github)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-discover-my-major)

;;----------------------------------------------------------------------------
;; Load prog language support
;;----------------------------------------------------------------------------
(require 'init-lsp)
;;(require 'init-eglot)

;;----------------------------------------------------------------------------
;; Load prog language
;;----------------------------------------------------------------------------
(require 'init-python)
(require 'init-elixir)
(require 'init-elm)
(require 'init-rust)
(require 'init-haskell)
(require 'init-javascript)
(require 'init-clojure)
(require 'init-ruby)
(require 'init-scala)
(require 'init-web)

;;----------------------------------------------------------------------------
;; Load misc configs
;;----------------------------------------------------------------------------
(require 'init-recentf)
(require 'init-org)
(require 'init-games)

;;----------------------------------------------------------------------------
;; Document readers
;;----------------------------------------------------------------------------
(require 'init-nov)
(require 'init-pdf)


(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;; Start emacs by default using the following directory
(setq default-directory "~/Code/")



(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
