;;; init-clojure.el --- Cider support for clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :hook (clojure-mode . subword-mode)
  :config
  (use-package cljsbuild-mode)
  (use-package elein)

  (use-package cider
    :hook (cider-repl-mode . subword-mode)
    :init
    (setq nrepl-popup-stacktraces nil))

  (use-package flycheck-clojure
    :config
    (flycheck-clojure-setup)))



(provide 'init-clojure)
;;; init-clojure.el ends here
