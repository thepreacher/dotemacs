;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Always load newest byte code
(setq load-prefer-newer t)

;;; Standard package repositories

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Official MELPA Mirror, in case necessary.
  ;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)
  (if (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))
    (unless no-ssl
      ;; Force SSL for GNU ELPA
      (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (version= "26.2" emacs-version)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Fire up package.el
(require 'cl-lib)
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))

;; To aid benchmarking
;; Show a message whenever a package takes longer than 0.1s to load
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package gnu-elpa-keyring-update)

(use-package async
  :config
  (async-bytecomp-package-mode 1))

(use-package use-package-ensure-system-package) ; Allows the use of :ensure-system-package keyword


(use-package diminish)

(provide 'init-elpa)
;;; init-elpa.el ends here
