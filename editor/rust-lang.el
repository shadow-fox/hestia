;;; rust-lang.el --- Rust Language Setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rust-mode
  :config
  ;; comment to disable rustfmt on save
  (setq rust-format-on-save t)
  (setq rust-lsp-client 'eglot)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  (rust-mode . (lambda () (prettify-symbols-mode)))
  (rust-mode . eglot-ensure))

(provide 'rust-lang)

;;; rust-lang.el ends here
