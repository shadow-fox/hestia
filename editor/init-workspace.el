;;; init-workspace.el --- Summary:

;;; Commentary:

;;; Code:

(use-package persp-mode
  :defer t
  :straight t)

(use-package ace-window
  :defer t
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package perspective
  :defer t
  :straight t
  :custom
  (persp-mode-prefix-key "C-x w")
  (persp-state-default-file (concat hestia-dir "persp"))
  :config
  (persp-mode))

(general-define-key
 "M-o" 'ace-window)

(provide 'init-workspace)
;;; init-workspace.el ends here
