(use-package paradox
  :commands package-list-packages
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  (paradox-spinner-type 'progress-bar)
  (paradox-github-token t)
  :config
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))


(provide 'init-paradox)
