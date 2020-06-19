;;; Org
;; OrgPac
(use-package org
  :ensure nil
  :defer t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)
  (:map org-mode-map ("C-c C-p" . org-export-as-pdf-and-open))
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  (org-agenda-window-setup 'other-window)
  :config
  (unless (version< org-version "9.2")
    (require 'org-tempo))
  (when (file-directory-p "~/org/agenda/")
    (setq org-agenda-files (list "~/org/agenda/")))

  (defun org-export-turn-on-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

  (defun org-export-as-pdf-and-open ()
    "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
    (interactive)
    (save-buffer)
    (let* ((pdf-path (org-latex-export-to-pdf))
           (pdf-name (file-name-nondirectory pdf-path)))
      (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
          (progn
            (kill-matching-buffers (concat "^" pdf-name) t t)
            (find-file-other-window pdf-name))
        (find-file-other-window pdf-name))
      (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex")))))
;; -OrgPac

;; TocOrgPac
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))
;; -TocOrgPac

;; HTMLIZEPac
(use-package htmlize :ensure t :defer t)
;; -HTMLIZEPac

;; OXGFMPac
(use-package ox-gfm :ensure t :defer t)
;; -OXGFMPac

;; PlantUMLPac
(use-package plantuml-mode
  :ensure t
  :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t))))

;; Org Bullets
(use-package org-bullets
  :ensure t
  :defer t
  :commands org-bullets-mode
  :hook ((org-mode . org-bullets-mode))
  :config
  (setq org-bullets-bullet-list '("∙")))

(provide 'init-org)
