;;; init-highlights.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize Constants
;; Compatibility: emacs-version >= 26.1
;;
;;; Commentary:
;; Gui icons to enhance file listing
;;; Code:

(use-package highlight-indent-guides
  :delight
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character 9615) ; left-align vertical bar
  (highlight-indent-guides-auto-character-face-perc 20))

(use-package highlight-symbol
  :delight
  :hook (prog-mode . highlight-symbol-mode)
  :custom-face
  (highlight-symbol-face ((t (:background "#44475a"))))
  :custom
  (highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package beacon
  :init
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  :config
  (beacon-mode))

;; hightlight current line
(global-hl-line-mode +1)

;; visual-fill-column
(use-package visual-fill-column
  :hook ((visual-line-mode . visual-fill-column-mode)))

;; Display line numbers
(setq-default display-line-numbers-type 'relative ;; for relative numbering set this to 'visual or 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

(set-face-attribute 'line-number nil
                    :font "OperatorMono NF")

(set-face-attribute 'line-number-current-line nil
                    :weight 'bold
                    :font "OperatorMono NF"
                    :foreground "goldenrod")
                    ;:background "slate gray"

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


(provide 'init-highlights)
;; Local Variables:
;;; init-highlights.el ends here
