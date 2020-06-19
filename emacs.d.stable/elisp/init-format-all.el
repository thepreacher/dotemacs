;;; init-format-all.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize format-all
;; https://github.com/lassik/emacs-format-all-the-code
;;; Commentary:
;;
;;; Code:

(use-package format-all
  :hook ((
           js-mode
           js2-mode
           python-mode
           ;elixir-mode
           elm-mode
           haskell-mode
           html-mode) . format-all-mode))


(provide 'init-format-all)
;; Local Variables:
;;; init-format-all.el ends here
