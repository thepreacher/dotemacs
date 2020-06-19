;;; init-search.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Constants
;; Compatibility: emacs-version >= 26.1
;;
;;; Commentary:
;; Searching/sorting enhancements & project management
;;; Code:

(use-package amx)

(use-package flx)

(use-package flx-ido
  :after ido
  :init
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode 1))

;; Ido vertical
(use-package ido-vertical-mode
  :after ido
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode 1))

;; Find file in project
(use-package find-file-in-project
  :defer t)

;; Projectile
(use-package projectile
  :delight
  :init
  (setq projectile-require-project-root nil)
  (setq
    projectile-project-search-path '("~/projects/learn/elixir"
                                      "~/projects/learn/python"
                                      "~/projects/learn/elm"
                                      "~/projects/learn/rust"
                                      "~/projects/learn/ai"))
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")
  :custom
  (projectile-sort-order 'recentf)
  (projectile-indexing-method 'hybrid)
  (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (use-package counsel-projectile
    :config
    (counsel-projectile-mode))
  (use-package ibuffer-projectile))


;; Ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-display-style nil
                ivy-virtual-abbreviate 'abbreviate
                ivy-initial-inputs-alist nil
                enable-recursive-minibuffers t
                ivy-use-selectable-prompt t
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-count-format "(%d/%d) "     ; Show current match and matches
                ivy-extra-directories nil) ; Do not show "./" and "../"
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-M-j" . ivy-immediate-done))
  :config
  (setq ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (counsel-projectile-ag . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (amx-completing-read-ivy . ivy--regex-fuzzy)
                                (counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-fuzzy))))

;; Swiper
(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper))
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t))

;; Counsel
;; https://github.com/abo-abo/swiper/issues/575
;; How to create new file with counsel-find-file when there are candidates for regexp?
;; Press C-M-j instead of RET to use the literal text entered instead of the current match.
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ;("C-z t c" . counsel-load-theme)
   ("C-h b" . counsel-descbinds)
   ("M-y" . counsel-yank-pop)
   ("C-x r b" . counsel-bookmark)
   ("C-h a" . counsel-apropos)
   ("C-h S" . counsel-info-lookup-symbol)
   ("C-h u" . counsel-unicode-char)
   ("M-?" . sanityinc/counsel-search-project)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
  :config
  (setq-default ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^")))
  (when (require 'projectile)
    (let ((search-function
           (cond
            ((executable-find "rg") 'counsel-rg)
            ((executable-find "ag") 'counsel-ag)
            ((executable-find "pt") 'counsel-pt)
            ((executable-find "ack") 'counsel-ack))))
      (when search-function
        (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
          "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
          (interactive (list (let ((sym (thing-at-point 'symbol)))
                               (when sym (regexp-quote sym)))
                             current-prefix-arg))
          (let ((current-prefix-arg)
                (dir (if use-current-dir
                         default-directory
                       (condition-case err
                           (projectile-project-root)
                         (error default-directory)))))
            (funcall search-function initial-input dir)))))
    (with-eval-after-load 'ivy
      (add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))))


(use-package ivy-posframe
  :after ivy
  :delight
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-height-alist '((t . 20)))
  (ivy-posframe-parameters '((internal-border-width . 1))) ;; Value changed from 10 to 1 for emacs-mac
  (ivy-posframe-width 70)
  :config
  (ivy-posframe-mode +1))

;; select from xref candidates with ivy
(use-package ivy-xref
  :after ivy
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package prescient
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy)
  :custom
  (ivy-prescient-sort-commands
   '(:not swiper
          counsel-grep
          counsel-rg
          counsel-projectile-rg
          ivy-switch-buffer
          counsel-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

(provide 'init-search)
;; Local Variables:
;;; init-search.el ends here
