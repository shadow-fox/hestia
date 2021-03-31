;;; init-project.el --- Summary

;;; Commentary:

;;; Code:

(use-package projectile
  :straight t
  :defer t
  :hook
  (after-init-hook . projectile-mode)
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (setq-default projectile-generic-command "rg --files --hidden")
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
  (setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (setq projectile-project-search-path '("/storage/sfox/Sandbox/System/wip"
					                     "/storage/sfox/Sandbox/org/"
					                     "/storage/sfox/Sandbox/B-ara"
					                     "/storage/sfox/Sandbox/OSS/"
					                     "/storage/sfox/Sandbox/Work/")))


(general-define-key
 "C-x p" '(
           :keymap projectile-command-map
           :package projectile
           ))

(provide 'init-project)
;;; init-project.el ends here
