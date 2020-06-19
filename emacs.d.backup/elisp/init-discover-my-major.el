;;; init-discover-my-major.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-discover-my-major.el
;; Description: Initialize Discover-My-Major
;;
;;; Commentary:
;;
;; This initializes discover-my-major
;;

;;
;;; Code:

;; DiscMyMajor
(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))
;; -DiscMyMajor

(provide 'init-discover-my-major)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-discover-my-major.el ends here
