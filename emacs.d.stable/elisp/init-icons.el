;;; init-icons.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Constants
;; Compatibility: emacs-version >= 26.1
;;
;;; Commentary:
;; Gui icons to enhance file listing
;;; Code:

;; NOTE: if it’s the first time that you install the package, you must run M-x all-the-icons-install-fonts.
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-spacer " "))

(provide 'init-icons)
;; Local Variables:
;;; init-icons.el ends here
