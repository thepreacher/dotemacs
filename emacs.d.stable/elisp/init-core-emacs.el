;;; init-core-emacs.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Constants
;; Compatibility: emacs-version >= 26.1
;;
;;; Commentary:
;; Configure stock emacs without third party package apart from use-packages
;;; Code:
(eval-when-compile
  (require 'init-const))

;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(use-package emacs
  :preface
  (defvar napo/indent-width 4)
  :init
  (setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
  :custom
  (user-full-name "Napoleon Ahiable")
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (auto-save-default nil)
  (cursor-type 'bar)
  (use-file-dialog nil)
  (use-dialog-box nil)
  (inhibit-startup-screen t)
  (initial-scratch-message ";; Happy Hacking!!!\n")
  (mouse-yank-at-point t)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)
  (scroll-preserve-screen-position 'always)
  (set-mark-command-repeat-pop t)
  (cua-selection-mode t)     ; for rectangles, CUA is nice
  (electric-indent-mode 1)   ; Indentation behaviour
  (mouse-drag-copy-region t) ; Automatically copy text selected with the mouse
  (bookmark-default-file (no-littering-expand-etc-file-name "bookmarks.el"))
  :config
  ;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;(add-to-list 'default-frame-alist '(ns-appearance . dark))
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
  (setq-default line-spacing 3
            indent-tabs-mode nil
            tab-width napo/indent-width)

  ;; DisableUnnecessaryInterface
  (unless (and (display-graphic-p) (eq system-type 'darwin))
   (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)

  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border)))


;;; Built-in packages

;; Frame
(use-package frame
  :ensure nil
  :custom
  ;; (initial-frame-alist '((fullscreen . maximized)))
  (initial-frame-alist '((top . 0) (left . 0) (width . 87) (height . 75))) ;; left value 748 for regular emacs
  (default-frame-alist '((top . 0) (left . 0) (width . 87) (height . 75))) ;; left value 748 for regular emacs

  ;; (add-to-list 'initial-frame-alist '(width  . 87))
  ;; (add-to-list 'initial-frame-alist '(height . 54))
  ;; (add-to-list 'default-frame-alist '(width  . 87))
  ;; (add-to-list 'default-frame-alist '(height . 54))
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
  :config
  (blink-cursor-mode -1)
  (when (member "JetBrainsMono " (font-family-list))
    (set-frame-font "OperatorMono NF-14:weight=regular" t t))
  (defun my/disable-scroll-bars (frame)
    (modify-frame-parameters frame
                             '((vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil)
                               (left . 0))))
  (add-hook 'after-make-frame-functions 'my/disable-scroll-bars))

;; Startup
(use-package "startup"
  :ensure nil
  :custom
  (inhibit-startup-screen t))

;; Custom File
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Simple
(use-package simple
  :ensure nil
  :config
  (column-number-mode +1))

;; Window
(use-package "window"
  :ensure nil
  :preface
  (defun napo/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun napo/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (global-set-key (kbd "C-x 2") #'napo/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'napo/split-and-follow-vertically))

;; Delete Selection
(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

;; Files
(use-package files
  :ensure nil
  :custom
  (confirm-kill-processes nil)
  (make-backup-files nil))

;; Autorevert
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode +1))

;; Eldoc
(use-package eldoc
  :ensure nil
  :delight
  :hook (prog-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 0.4))

;; Javacript
(use-package js
  :ensure nil
  :defer t
  :custom
  (js-indent-level napo/indent-width))

;; Xref
(use-package xref
  :ensure nil
  :config
  (define-key prog-mode-map (kbd "s-b") #'xref-find-definitions)
  (define-key prog-mode-map (kbd "s-[") #'xref-pop-marker-stack))

;; CC-vars
(use-package cc-vars
  :ensure nil
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "k&r")))
  :config
  (setq-default c-basic-offset napo/indent-width))

;; Prolog
(use-package prolog
  :ensure nil
  :mode (("\\.pl\\'" . prolog-mode))
  :custom
  (prolog-indent-width napo/indent-width))

;; Python
(use-package python
  :ensure nil
  :defer t
  :custom
  (python-indent-offset napo/indent-width)
  (python-shell-interpreter "python3"))

;; MWheel
(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))

;; Parens
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :custom-face
  (show-paren-match ((t (:foreground "#ffffff" :underline t))))
  :config
  (show-paren-mode +1))

;; Ediff
(use-package ediff
  :ensure nil
  :init
  (setq-default ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain))

;; Faces
(use-package faces
  :ensure nil
  :preface
  (defun napo/disable-bold-and-fringe-bg-face-globally ()
    "Disable bold face and fringe background in Emacs."
    (interactive)
    (set-face-attribute 'fringe nil :background nil)
    (mapc #'(lambda (face)
              (when (eq (face-attribute face :weight) 'bold)
                (set-face-attribute face nil :weight 'normal))) (face-list)))
  :config
  (add-hook 'after-init-hook #'napo/disable-bold-and-fringe-bg-face-globally))

;; Flyspell
(require 'ispell)

(use-package flyspell
  :ensure nil
  :if (executable-find ispell-program-name)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-program-name "/usr/local/bin/aspell"))

;; Electric pair
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

;; Whitespace
(use-package whitespace
  :ensure nil
  :preface
  (defun sanityinc/show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t))
  :config
  (setq-default show-trailing-whitespace nil)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook 'sanityinc/show-trailing-whitespace)))

;; Dired
(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t))

;; Saveplace
(use-package saveplace
  :config
  (save-place-mode +1))

;; Recentf
(use-package recentf
  :custom
  (dired-dwim-target t)
  :config
  ;(setq recentf-save-file (expand-file-name "recentf" autosaves))
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-exclude '("/tmp/" "/ssh:" no-littering-var-directory no-littering-etc-directory)
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))


;; Uniquify
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-show-empty-filter-groups nil)
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

;; Org
(use-package org
  :ensure nil
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)
  (:map org-mode-map ("C-c C-p" . org-export-as-pdf-and-open))
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  (org-agenda-window-setup 'other-window)
  :config
  (unless (version< org-version "9.2")
    (require 'org-tempo))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))

  (defun org-export-turn-on-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

  (defun org-export-as-pdf-and-open ()
    "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
    (interactive)
    (save-buffer)
    (let* ((pdf-path (org-latex-export-to-pdf))
           (pdf-name (file-name-nondirectory pdf-path)))
      (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
          (progn
            (kill-matching-buffers (concat "^" pdf-name) t t)
            (find-file-other-window pdf-name))
        (find-file-other-window pdf-name))
      (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex")))))

(use-package prettify-symbols-mode
  :ensure nil
  :hook ((after-init . global-prettify-symbols-mode)
         (clojure-mode . my-add-pretty-lambda)
         (haskell-mode . my-add-pretty-lambda))
  :config
  (defun my-add-pretty-lambda ()
    "make some word or string show as pretty Unicode symbols"
    (setq prettify-symbols-alist
          '(
            ("lambda" . 955))))) ; λ

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Start emacs by default using the following directory
(setq default-directory (expand-file-name "~/projects/"))

(provide 'init-core-emacs)
;; Local Variables:
;;; init-core-emacs.el ends here
