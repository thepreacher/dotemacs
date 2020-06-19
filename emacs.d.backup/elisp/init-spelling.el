;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary: REF: https://www.reddit.com/r/emacs/comments/8rxm7h/tip_how_to_better_manage_your_spelling_mistakes/

;;; Code:

(require 'ispell)

(use-package abbrev
  :defer 1
  :init
  (setq save-abbrevs 'silently)
  :custom
  (abbrev-file-name (expand-file-name ".abbrev_defs" user-emacs-directory))
  (abbrev-mode 1)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package flyspell
  :if (executable-find ispell-program-name)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct-ivy
  :after flyspell
  :ensure t
  :bind (:map flyspell-mode-map)
        ("C-;" . flyspell-correct-word-generic)
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))





(provide 'init-spelling)
;;; init-spelling.el ends here
