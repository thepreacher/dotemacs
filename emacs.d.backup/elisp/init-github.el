;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-git-magit)

(use-package yagist :ensure t :defer t)
(use-package bug-reference-github
  :ensure t
  :defer t
  :hook
  (prog-mode-hook . bug-reference-prog-mode))

(use-package github-clone :ensure t :defer t)
(use-package forge :ensure t :defer t)
(use-package github-review :ensure t :defer t)


(provide 'init-github)
;;; init-github.el ends here
