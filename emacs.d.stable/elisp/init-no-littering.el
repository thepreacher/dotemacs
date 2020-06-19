;; Help keeping ~/.emacs.d clean
(use-package no-littering)

; Auto-save settings
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(provide 'init-no-littering)
;; Local Variables:
;;; init-no-littering.el ends here
