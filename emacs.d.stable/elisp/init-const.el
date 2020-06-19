;;; init-const.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Constants
;; Compatibility: emacs-version >= 26.1
;;
;;; Commentary:
;; This initializes constants
;;; Code:

;; Consts
(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *sys/root*
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *python*
  (executable-find "python")
  "Do we have python?")

(defconst *python3*
  (executable-find "python3")
  "Do we have python3?")

(defconst *tr*
  (executable-find "tr")
  "Do we have tr?")

(defconst *mvn*
  (executable-find "mvn")
  "Do we have Maven?")

(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst *gcc*
  (executable-find "gcc")
  "Do we have gcc?")

(defconst *git*
  (executable-find "git")
  "Do we have git?")

(defconst *pdflatex*
  (executable-find "pdflatex")
  "Do we have pdflatex?")

(defconst *eaf-env*
  (and *sys/linux* *sys/gui* *python3*
       (executable-find "pip")
       (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  "Check basic requirements for EAF to run.")

(defconst no-littering-etc-dir
  (expand-file-name "etc" user-emacs-directory))

(defconst no-littering-var-dir
  (expand-file-name "var" user-emacs-directory))

(defconst no-littering-var-lsp-dir
  (expand-file-name "var/lsp" user-emacs-directory))
;; -Consts


(provide 'init-const)
;;; init-const.el ends here
