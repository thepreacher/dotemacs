;;; init-git-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(eval-when-compile
  (require 'init-const))

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(use-package git-blamed :ensure t :defer t)
(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package git-timemachine
  :ensure t
  :defer t
  :bind (("C-x v t"  . git-timemachine-toggle)))

; The best Git client out there
(use-package magit
  :ensure t
  :if *git*
  :bind (([(meta f12)] . magit-status)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         (:map magit-mode-map
               ("C-M-<up>" . magit-section-up)))
  :config
  (setq-default magit-diff-refine-hunk t)
  (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
        magit-branch-prefer-remote-upstream '("master")
        magit-completing-read-function 'ivy-completing-read
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18)
        magit-refs-show-commit-count 'all
        magit-save-repository-buffers 'dontask
        magit-section-visibility-indicator nil)

  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))

  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file))

  (use-package fullframe
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window))

  (use-package  magit-todos :ensure t)

  (use-package git-commit
    :ensure t
    :hook (commit-mode . goto-address-mode)))


(when *sys/mac*
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; Convenient binding for vc-git-grep
(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

(autoload 'vc-git-root "vc-git")


(provide 'init-git-magit)
;;; init-git-magit.el ends here
