;;; init-rust.el --- Rust editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package toml-mode)

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after (rust-mode)
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'init-rust)
;;; init-rust.el ends here
