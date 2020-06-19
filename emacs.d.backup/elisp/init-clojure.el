;;; init-clojure.el --- Cider support for clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :ensure t
  :defer t
  :hook (clojure-mode . subword-mode)
  :config
  (use-package cljsbuild-mode :ensure t)
  (use-package elein :ensure t))


(use-package cider
  :ensure t
  :after clojure-mode
  :hook (cider-repl-mode . subword-mode)
  :init
  (setq nrepl-popup-stacktraces nil))


(use-package flycheck-clojure
  :ensure t
  :after (clojure-mode cider flycheck)
  :config
  (flycheck-clojure-setup))



(provide 'init-clojure)
;;; init-clojure.el ends here
