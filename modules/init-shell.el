;;; init-shell.el --- Summary

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package envrc
  :straight t
  :commands (envrc-allow)
  :config
  (envrc-global-mode))

(use-package vterm
  :straight t
  :commands (vterm)
  :config
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)
  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000))

(general-define-key
 :keymaps 'envrc-mode-map
 "C-c e" 'envrc-command-map)

(provide 'init-shell)
;;; init-shell.el ends here
