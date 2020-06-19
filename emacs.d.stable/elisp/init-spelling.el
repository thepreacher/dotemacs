;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary: REF: https://www.reddit.com/r/emacs/comments/8rxm7h/tip_how_to_better_manage_your_spelling_mistakes/

;;; Code:
(use-package abbrev
  :defer 5
  :init
  (setq save-abbrevs 'silently)
  :config
  (setq abbrev-file-name (expand-file-name ".abbrev_defs" user-emacs-directory))
  (abbrev-mode 1)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package flyspell-correct-ivy
  :bind (:map flyspell-mode-map)
        ("C-;" . flyspell-correct-wrapper)
  :config (flyspell-correct-interface 'flyspell-correct-ivy))


(provide 'init-spelling)
;;; init-spelling.el ends here
