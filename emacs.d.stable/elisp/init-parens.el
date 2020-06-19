;;; Commentary: REF: https://ebzzry.io/en/emacs-pairs/
;;; Code

;; https://github.com/Fuco1/smartparens/issues/80
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :init
  ;; https://stackoverflow.com/questions/23789962/how-to-disable-emacs-highlighting-whitespace-in-parenthesis
  (setq sp-highlight-pair-overlay nil)
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-M-e" . sp-up-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-k" . sp-change-enclosing)
        ("M-k" . sp-kill-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange))
  :config
  (require 'smartparens-config)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil)
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

; (use-package simple-paren
;   :config
;   (simple-paren-define left-pointing-single-arrow ?<-)
;   (simple-paren-define angled-bracket-percent-pair ?<% ?%>)
;   (simple-paren-define angled-bracket-equal-pair ?<%= ?%>))


(provide 'init-parens)
;;; init-parens.el ends here
