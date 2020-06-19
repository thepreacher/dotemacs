;;; init-pdf.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-pdf.el
;; Description: Initialize pdf-tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

;; PDFToolsPac
(use-package pdf-tools-install
  :ensure pdf-tools
  :if (and *sys/gui* (not *sys/win32*))
  :mode "\\.pdf\\'"
  :commands (pdf-loader-install)
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-loader-install))
;; -PDFToolsPac

(provide 'init-pdf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pdf.el ends here
