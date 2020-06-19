;; Description: Create the no-littering directories
;
;; Code

; Etc
(unless (file-exists-p no-littering-etc-dir)
  (make-directory no-littering-etc-dir))

; Var
(unless (file-exists-p no-littering-var-dir)
  (make-directory no-littering-var-dir))

; Lsp
(unless (file-exists-p no-littering-var-lsp-dir)
  (make-directory no-littering-var-lsp-dir))


(provide 'init-no-littering-dirs)
;; early-init-no-littering.el ends here
