(use-package paradox
  :ensure t
  :commands package-list-packages
  :init
  (setq paradox-automatically-star nil
        paradox-display-star-count nil
        paradox-execute-asynchronously nil
        paradox-github-token t
        paradox-use-homepage-buttons nil
        paradox-spinner-type 'progress-bar))

(provide 'init-paradox)
