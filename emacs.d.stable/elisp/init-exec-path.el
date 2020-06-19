;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  ;(setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (progn
    (dolist (var '("SSH_AUTH_SOCK"
                   "LANG"
                   "WORKON_HOME"))
      (add-to-list 'exec-path-from-shell-variables var)))
  (exec-path-from-shell-initialize))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
