;;; init-nov.el --- Configure epub reader -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.0))

(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode))
  :hook (
         (nov-mode . my-nov-font-setup)
         (nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode))
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t))


(provide 'init-nov)
;;; init-nov.el ends here
