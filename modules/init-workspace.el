;;; init-workspace.el --- Summary:

;;; Commentary:

;;; Code:

(use-package ace-window
  :defer t
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(general-define-key
 "M-o" 'ace-window)

(provide 'init-workspace)
;;; init-workspace.el ends here
