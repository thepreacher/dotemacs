;;; init.el --- Emacs init file
;;  Author: Napoleon Ahiable
;;; Commentary:
;;  This is my personal Emacs configuration
;; Installation: brew install emacs-plus --HEAD --without-spacemacs-icon --with-jansson
;; Reference dot emacs.d
;;    https://github.com/ianpan870102/.personal-emacs.d
;;    https://github.com/purcell/emacs.d
;;    https://github.com/MatthewZMD/.emacs.d
;;    https://github.com/acowley/dotfiles/blob/master/emacs
;;
;;; Code:

;; Produce backtraces when errors occur
;(setq debug-on-error t)

;; Disable these early
(tool-bar-mode -1)
(scroll-bar-mode -1)

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


(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)

(defvar napo/gc-cons-threshold 20000000)


(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold napo/gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold napo/gc-cons-threshold)))

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

;; Constants
(require 'init-const)

;;----------------------------------------------------------------------------
;; ;; Initialise packages configs
;;----------------------------------------------------------------------------
(require 'init-elpa)
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; General emacs configs
;;----------------------------------------------------------------------------
(require 'init-themes)
(require 'init-no-littering-dirs)
(require 'init-no-littering)
(require 'init-frame-hooks)
(require 'init-core-emacs)
(require 'init-macosx)
;(require 'init-fonts)
(require 'init-modeline)
(require 'init-icons)
;(require 'init-dashboard)
(require 'init-highlights)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-snippets)
(require 'init-misc)

;;----------------------------------------------------------------------------
;; General utilites
;;----------------------------------------------------------------------------
(require 'init-git)
;(require 'init-evil)
(require 'init-search)

;;----------------------------------------------------------------------------
;; Programming language support and utilities
;;----------------------------------------------------------------------------
(require 'init-flycheck)
;(require 'init-parens)
(require 'init-lsp)
;;(require 'init-eglot)
(require 'init-company)
(require 'init-paradox)
(require 'init-format-all)

;;----------------------------------------------------------------------------
;; Programming language
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
(require 'init-go)
(require 'init-slime)
(require 'init-web)
(require 'init-markdown)
(require 'init-yaml)


(require 'init-nov)
(require 'init-pdf)

;;----------------------------------------------------------------------------
;; Jupyter Notebook integration
;;----------------------------------------------------------------------------
(require 'init-ein)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
