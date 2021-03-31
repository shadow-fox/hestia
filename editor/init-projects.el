;;; init-projects.el --- Summary:

;;; Commentary:

;;; Code:

;; (use-package project-x
;;   :straight (:host github :type git :repo "karthink/project-x")
;;   :after project
;;   :config
;;   (add-hook 'project-find-functions 'project-x-try-local 90)
;;   (add-hook 'kill-emacs-hook 'project-x--window-state-write)
;;   (add-to-list 'project-switch-commands
;;                '(?j "Restore windows" project-x-windows) t))

(general-define-key
 "s-p w" 'project-x-window-state-save
 "s-p l" 'project-x-window-state-load)

(use-package projectile
  :straight t
  :commands (projectile-switch-project)
  :init
  (projectile-mode t)
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (setq-default projectile-generic-command "rg --files --hidden")
  (setq projectile-sort-order 'recently-active)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
  (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (setq projectile-project-search-path '("/storage/sfox/Sandbox/System/wip/"
					 "/storage/sfox/Sandbox/org/"
					 "/storage/sfox/Sandbox/B-ara/"
					 "/storage/sfox/Sandbox/OSS/"
					 "/storage/sfox/Sandbox/Work/"))
  )

(general-define-key
 "s-p p" 'projectile-switch-project)

(use-package ace-window
  :defer t
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(general-define-key
 "M-o" 'ace-window)

;; (use-package counsel-projectile
;;   :straight t
;;   :after projectile
;;   :config
;;   ;; no highlighting visited files; slows down the filtering
;;   (ivy-set-display-transformer #'counsel-projectile-find-file nil)
;;   (counsel-projectile-mode t))

;; (use-package ibuffer-projectile
;;   :straight t
;;   :config
;;   (add-hook 'ibuffer-hook
;;     (lambda ()
;;       (ibuffer-projectile-set-filter-groups)
;;       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;         (ibuffer-do-sort-by-alphabetic))))
;;   (setq ibuffer-formats
;;       '((mark modified read-only " "
;;               (name 18 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " "
;;               project-relative-file))))

(use-package org-projectile
  :after (org-mode)
  :straight t
  :config
  (progn
    (org-projectile-per-project)
    ;; (setq org-projectile-projects-file "/storage/sfox/Sandbox/org/projects.org")
    (setq org-projectile-per-project-filepath "Project.org")
    ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    ;;(setq org-agenda-files (seq-filter 'file-readable-p (delete-dups (append org-agenda-files (org-projectile-todo-files)))))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(provide 'init-projects)
;;; init-projects.el ends here
