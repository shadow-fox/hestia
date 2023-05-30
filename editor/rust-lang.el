;;; rust-lang.el --- Rust Language Setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rustic
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  :hook
  (rust-mode-hook 'eglot-ensure))

(provide 'rust-lang)

;;; rust-lang.el ends here
