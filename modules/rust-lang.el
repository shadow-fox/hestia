;;; rust-lang.el --- Rust Language Setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rustic
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save-method t)
  (setq rustic-lsp-client 'eglot))

(provide 'rust-lang)

;;; rust-lang.el ends here
