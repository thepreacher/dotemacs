;;; init-scala.el --- support for Clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package scala-mode
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         (("\\.sc\\'" . scala-mode)))
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :after (scala-mode ensime)
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
    'minibuffer-complete-word
    'self-insert-command
    minibuffer-local-completion-map))





(provide 'init-scala)
;;; init-scala.el ends here
