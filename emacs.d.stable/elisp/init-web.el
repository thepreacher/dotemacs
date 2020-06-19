;;; init-web-mode.el --- Mixed template/language editing support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package emmet-mode
  :hook ((web-mode html-mode css-mode sgml-mode) . emmet-mode)
  :init
  (setq emmet-move-cursor-between-quotes t))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify)

;;; HTML completion
(use-package company-web
  :after company
  :bind (("C-c w" . company-web-html))
  :config
  (add-to-list 'company-backends 'company-web-html))
;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
       (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

(add-to-list 'auto-mode-alist
             '("\\.l?eex\\'" . (lambda ()
                               ;; add major mode setting here, if needed, for example:
                               ;; (text-mode)
                                (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))


;;; Web mode
(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.l?eex\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsx$" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  ;; make web-mode play nice with smartparens
  (setq web-mode-enable-auto-pairing nil)
  ;(setq web-mode-enable-auto-closing nil)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))
  ; :config
  ; (sp-with-modes 'web-mode
  ;   (sp-local-pair "% " " %"
  ;                  :unless '(sp-in-string-p)
  ;                  :post-handlers '(((lambda (&rest _ignored)
  ;                                      (just-one-space)
  ;                                      (save-excursion (insert " ")))
  ;                                    "SPC" "=" "#")))
  ;   (sp-local-pair "<% " " %>" :insert "C-b %")
  ;   (sp-local-pair "<%= " " %>" :insert "C-b =")
  ;   (sp-local-pair "<%# " " %>" :insert "C-b #")))


(provide 'init-web)
;;; init-web-mode.el ends here
