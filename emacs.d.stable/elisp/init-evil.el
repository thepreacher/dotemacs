(use-package evil
  :delight undo-tree-mode
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-shift-width napo/indent-width)
  :hook (after-init . evil-mode)
  :preface
  (defun napo/save-and-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil))
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'napo/save-and-kill-this-buffer))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :delight
  :config
  (evil-commentary-mode +1))

(use-package evil-magit)

(provide 'init-evil)
;; Local Variables:
;;; init-evil.el ends here