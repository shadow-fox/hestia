;;; init-project.el --- Summary

;;; Commentary:

;;; Code:

(use-package projectile
  :straight t
  :hook
  (after-init-hook . projectile-mode)
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (setq-default projectile-generic-command "rg --files --hidden")
  ;;(setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("/storage/sfox/Sandbox/System/wip"
					 "/storage/sfox/Sandbox/org/"
					 "/storage/sfox/Sandbox/B-ara"
					 "/storage/sfox/Sandbox/OSS/"
					 "/storage/sfox/Sandbox/Work/")))

(use-package ibuffer-projectile
  :straight t
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              project-relative-file))))

(use-package org-projectile
  :after org
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

(general-define-key
 "C-x p" '(
           :keymap projectile-command-map
           :package projectile
           ))

(provide 'init-project)
;;; init-project.el ends here
