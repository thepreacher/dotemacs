;;; init-dashboard.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dashboard.el
;; Description: Initialize Dashboard

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes dashboard
; FYI, a couple of notes on use-package that I find useful:
; You can define the my/dashboard-banner function in the :preface section to keep
; the code with the configuration in your use-package declaration.
; With recent versions of use-package you can use :hook to configure hooks. I find
; using :hook makes the configuration easier to read since all the hooks are in a single spot.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;
;;; Code:

(use-package dashboard
  :if (< (length command-line-args) 2)
  :preface
  (defun dashboard-load-packages (list-size)
    (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 5) ? )
            (format "%d packages loaded in %s" (length package-activated-list) (emacs-init-time))))
  :custom
  (dashboard-banner-logo-title "With Great Power Comes Great Responsibility")
  (dashboard-center-content t)
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-startup-banner 'logo)
  :config
  (setq-local beacon-mode nil)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(
          ((,(all-the-icons-octicon "mark-github" :height 1.2 :v-adjust -0.1)
             "Homepage"
             "Browse homepage"
             (lambda (&rest _) (browse-url "homepage")))
           (,(all-the-icons-material "update" :height 1.2 :v-adjust -0.24)
             "Update"
             "Update emacs"
             (lambda (&rest _) (browse-url "homepage")))
           ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))
  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))
  (dashboard-setup-startup-hook))



(provide 'init-dashboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
