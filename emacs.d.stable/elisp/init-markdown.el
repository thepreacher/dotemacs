(use-package markdown-mode
  ;:ensure-system-package (markdown pandoc)
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq flymd-markdown-regex (mapconcat 'identity '("\\.md\\'" "\\.markdown\\'" "markdown") "\\|"))

  ;; The default command for markdown (~markdown~), doesn't support tables
  ;; (e.g. GitHub flavored markdown). Pandoc does, so let's use that.
  (setq markdown-command "pandoc --from markdown --to html")
  (setq markdown-command-needs-filename t)
  (use-package flymd))


(provide 'init-markdown)
;; Local Variables:
;;; init-markdown.el ends here
