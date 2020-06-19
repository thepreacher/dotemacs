;; Advice - create directory, if not exists
; (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
;   "Create parent directory if not exists while visiting file."
;   (unless (file-exists-p filename)
;     (let ((dir (file-name-directory filename)))
;       (unless (file-exists-p dir)
;         (make-directory dir t)))))

(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))


(use-package diminish)

(use-package which-key
  :delight
  :custom
  (which-key-idle-delay 0.4)
  (which-key-idle-secondary-delay 0.4)
  :config
  (which-key-mode +1))

(use-package rainbow-mode
  :delight
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :delight
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "Gold"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "Orchid"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "LightSkyBlue"))))
  :custom
  (rainbow-delimiters-max-face-count 3)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; Dump jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)) ;; (setq dumb-jump-selector 'helm)

;; DiscMyMajor
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

;; A Collection of Ridiculously Useful eXtensions for Emacs. crux bundles a few
;;useful interactive commands to enhance your overall Emacs experience.
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("M-o" . crux-smart-open-line)
         ("M-O" . crux-smart-open-line-above)
         ("S-<return>" . crux-smart-open-line)))

(use-package dash                       ; A modern list library
  :defer t)

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("C-:" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

(use-package vlf                        ; View large files
  :defer t
  :config
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :hook (after-init . volatile-highlights-mode))

;; EditorConfig makes it easy to maintain the correct coding style when
;;switching between different text editors and between different projects.
(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :hook (after-init . whole-line-or-region-mode))

(use-package multiple-cursors
  :bind ("C-c m c" . 'mc/edit-lines))

;; Expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

; Add emoji support. This is useful when working with html.
(use-package emojify :defer t)

(use-package super-save
  :init
  (setq super-save-exclude '(".gpg"))
  (setq super-save-hook-triggers '(focus-out-hook))
  :config
  (super-save-mode +1))

(use-package info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))

(use-package zzz-to-char
  :bind ("M-z" . zzz-up-to-char))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))


(provide 'init-misc)
;;; init-misc.el ends here
