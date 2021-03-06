;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Flycheck
(use-package flycheck
  :preface
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  :hook ((prog-mode . flycheck-mode)
         (flycheck-mode . my/use-eslint-from-node-modules))
  :config
  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)
    :custom
    (flycheck-posframe-position 'point-bottom-left-corner))

  (use-package flycheck-color-mode-line
    :hook (flycheck-mode . flycheck-color-mode-line-mode))
  ;
  ; (use-package flycheck-inline
  ;   :hook (flycheck-mode . flycheck-inline-mode))

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
     '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(json-jsonlist)))

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
