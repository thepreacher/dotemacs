
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(provide 'init-git)
;; Local Variables:
;;; init-git.el ends here
