;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary: Reference for this setup is from https://ddavis.fyi/blog/eglot-python-ide/
;;; Code:

;; https://github.com/xhcoding/ms-python

;; Set default virtualenv location
(setenv "WORKON_HOME" (expand-file-name "~/.virtualenvs"))
(defalias 'workon 'pyvenv-workon)


(eval-when-compile
  (require 'init-flycheck)
  (require 'init-const))

;; PythonConfig
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
;; -PythonConfig

;; LSPPythonPac
(use-package lsp-python-ms
  :ensure t
  :after lsp-mode python
  :if (or *python3* *python*)
  :init
  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (expand-file-name "~/Code/github/lang_servers/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  ;; https://github.com/emacs-lsp/lsp-python-ms
  (setq lsp-python-ms-executable
        (expand-file-name "~/Code/github/lang_servers/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
  :custom
  (lsp-python-executable-cmd "python3"))
;; -LSPPythonPac


(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode))

;; Python code formatter
(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

;; Pipenv code formatter
(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(provide 'init-python)
;;; init-python.el ends here
