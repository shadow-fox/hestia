;;; init-org.el --- Summary

;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :straight t
  :config
  (setq org-hide-leading-stars nil)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "/storage/sfox/Sandbox/org/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "/storage/sfox/Sandbox/org/journal/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))))

(use-package org-superstar
  :straight t
  :after org-mode
  :hook (org-mode-hook . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s)
  ;; :custom
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

;; (use-package org-roam
;; ;;  :init
;; ;;  (setq org-roam-db-location "/storage/sfox/Sandbox/org/roam")
;;   :hook
;;   (after-init-hook . org-roam-mode)
;;   :config
;;   (setq org-roam-directory "/storage/sfox/Sandbox/org/roam"))

(defconst org-mode-leader "C-c o")

(general-create-definer org-mode-def
  :prefix org-mode-leader)

;; (org-mode-leader                        ;
;;  :keymaps 'org-mode-map)

(org-mode-def
 "c" 'org-capture
 "a" 'org-agenda)
  
(provide 'init-org)
;;; init-org.el ends here
