;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Ensure resizing Emacs window doesn't cause display problems
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Do not pop up *Messages* when clicking on the minibuffer
(bind-key [mouse-1] #'ignore minibuffer-inactive-mode-map)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Set the cursor as a vertical bar
(setq-default cursor-type 'bar)


;; Sensible defaults
(setq-default
  blink-cursor-interval 0.4
  bookmark-default-file (expand-file-name ".bookmarks.el" my-savefile-dir)
  buffers-menu-max-size 30
  case-fold-search t
  column-number-mode t
  delete-selection-mode t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  indent-tabs-mode nil
  auto-save-default nil
  mouse-yank-at-point t
  require-final-newline t
  save-interprogram-paste-before-kill t
  scroll-preserve-screen-position 'always
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  truncate-lines nil
  truncate-partial-width-windows nil
  backup-directory-alist `(("." . ,(concat user-emacs-directory
                                           "backups"))))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; hightlight current line
(global-hl-line-mode +1)

;; visual-fill-column
(use-package visual-fill-column
  :ensure t
  :hook ((visual-line-mode . visual-fill-column-mode)))

;; Display line numbers
(setq-default display-line-numbers-type 'relative ;; for relative numbering set this to 'visual or 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

(set-face-attribute 'line-number nil
                    :font "OperatorMono NF")

(set-face-attribute 'line-number-current-line nil
                    :weight 'bold
                    :font "OperatorMono NF"
                    :foreground "goldenrod")
                    ;:background "slate gray"

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'mode-line-bell-mode))

;; Indentation behavioiur
;;(electric-indent-mode 1)

;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(add-hook 'after-init-hook 'global-auto-revert-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(use-package symbol-overlay
  :ensure t
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :config
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Rectangle selections, and overwrite text when the selection is active
(cua-selection-mode t)                  ; for rectangles, CUA is nice

;; Handy key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;; Advice
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))


(use-package beacon
  :ensure t
  :init
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  :config
  (beacon-mode))

;; Which Key
(use-package which-key
  :ensure t
  :delight
  :init
  (setq-default which-key-idle-delay 1)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :ensure t)

;;; Currently only available in GNU Elpa
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; Dump jump
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)) ;; (setq dumb-jump-selector 'helm)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" my-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" my-savefile-dir))
  (savehist-mode +1))

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package multiple-cursors
  :ensure t
  :defer t)

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode))

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region
  :ensure t
  :hook (after-init . whole-line-or-region-mode))


;; Some local minor modes clash with CUA rectangle selection

(defvar-local sanityinc/suspended-modes-during-cua-rect nil
  "Modes that should be re-activated when cua-rect selection is done.")

(eval-after-load 'cua-rect
  (advice-add 'cua--deactivate-rectangle :after
              (lambda (&rest _)
                (dolist (m sanityinc/suspended-modes-during-cua-rect)
                  (funcall m 1)
                  (setq sanityinc/suspended-modes-during-cua-rect nil)))))

(defun sanityinc/suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (eval-after-load 'cua-rect
    (advice-add 'cua--activate-rectangle :after
                (lambda (&rest _)
                  (when (bound-and-true-p mode-name)
                    (push mode-name sanityinc/suspended-modes-during-cua-rect)
                    (funcall mode-name 0))))))

(sanityinc/suspend-mode-during-cua-rect-selection 'whole-line-or-region-local-mode)


(use-package highlight-escape-sequences
  :ensure t
  :hook (after-init . hes-mode))


;; A Collection of Ridiculously Useful eXtensions for Emacs. crux bundles a few
;;useful interactive commands to enhance your overall Emacs experience.
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-smart-kill-line)
         ("M-o" . crux-smart-open-line)
         ("M-O" . crux-smart-open-line-above)
         ("S-<return>" . crux-smart-open-line)))


(use-package dash                       ; A modern list library
  :ensure t
  :defer t)


(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)))

(use-package vlf                        ; View large files
  :ensure t
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
  :ensure t
  :defer t
  :config
  (volatile-highlights-mode +1))

;; EditorConfig makes it easy to maintain the correct coding style when
;;switching between different text editors and between different projects.
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
