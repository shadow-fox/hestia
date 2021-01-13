;;; init-shell.el --- Summary

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package envrc
  :straight t
  :config
  (envrc-global-mode))

(general-define-key
 :keymaps 'envrc-mode-map
 "C-c e" 'envrc-command-map)

(provide 'init-shell)
;;; init-shell.el ends here
