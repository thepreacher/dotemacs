;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

;; LSPPac
(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp lsp-deferred
  :init
  (setq-default lsp-restart 'auto-restart) ;; Otherwise will get the restart message on exit.
  (setq lsp-eslint-server-command
   '("node"
     "/Users/nahiable/.vscode/extensions/dbaeumer.vscode-eslint-2.0.13/server/out/eslintServer.js"
     "--stdio")) ;; This is required for eslint to work
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((java-mode python-mode go-mode
          js-mode js2-mode typescript-mode
          web-mode c-mode c++-mode objc-mode
          elixir-mode elm-mode rust-mode haskell-mode) . lsp))
;; -LSPPac

;; LSPUI
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if *sys/gui*
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
;; -LSPUI

;; DAPPac
(use-package dap-mode
  :ensure t
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle)))
  :hook ((after-init . dap-mode)
         (python-mode . (lambda () (require 'dap-python)))
         (haskell-mode . (lambda () (require 'dap-haskell)))
         (elm-mode . (lambda () (require 'dap-elm)))
         (rust-mode . (lambda () (require 'dap-rust)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode typescript-mode) . (lambda () (require 'dap-chrome))))
  :config
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode t))
  (dap-tooltip-mode t)
  (tooltip-mode t))
;; -DAPPac

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
