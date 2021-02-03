;;; init-prog.el --- Summary

;;; Commentary:

;;; Code:

(use-package flycheck
  :straight t
  :hook
  (after-init-hook . global-flycheck-mode))

(use-package flycheck-color-mode-line
  :straight t
  :hook
  (flycheck-mode-hook . flycheck-color-mode-line-mode)
  :after (flycheck))

(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-log-io t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :project)
  (setq lsp-modeline-code-actions-mode t)
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-enable-text-document-color t)
  (setq lsp-signature-render-documentation t)
  ;;  (setq lsp-headerline-breadcrumb-segments '(project file symbols))
  (setq lsp-enable-snippet t)
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil)))

(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :config
  (lsp-ui-mode t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-doc-enable t)
  :after (lsp))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(general-define-key
 :keymaps 'lsp-ui-mode-map
 [remap xref-find-references] 'lsp-ui-peek-find-references
 [remap xref-find-definitions] 'lsp-ui-peek-find-definitions)

(general-define-key
 "C-x g" 'magit-status
 "C-x M-g" 'magit-dispatch
 "C-c M-g" 'magit-file-dispatch)

(provide 'init-prog)
;;; init-prog.el ends here
