;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package amx
  :ensure t)

;; needed to tweak the matching algorithm used by ivy
(use-package flx
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-display-style 'fancy
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
  (require 'flx)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (amx-completing-read-ivy . ivy--regex-fuzzy)
          (t . ivy--regex-fuzzy))))


(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   :map ivy-mode-map
   ("M-s /" . swiper-thing-at-point)))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-z t c" . counsel-load-theme)
   ("C-h b" . counsel-descbinds)
   ("M-y" . counsel-yank-pop)
   ("C-x r b" . counsel-bookmark)
   ("C-h a" . counsel-apropos)
   ("C-h S" . counsel-info-lookup-symbol)
   ("C-h u" . counsel-unicode-char)
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
    (after-load 'ivy
      (add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))
    (global-set-key (kbd "M-?") 'sanityinc/counsel-search-project)))

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
  :ensure t
  :after ivy
  :config
  ;; Disable TRAMP buffers extended information to prevent slowdown
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbreviate
        ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 20))
            (ivy-rich-switch-buffer-size (:width 7 :align right))
            (ivy-rich-switch-buffer-indicators
             (:width 2 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 8 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))))

  (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :config
  (setq all-the-icons-spacer " ")
  (all-the-icons-ivy-setup))


(provide 'init-ivy)
;;; init-ivy.el ends here
