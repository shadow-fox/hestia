;;; lsp-setup.el --- LSP Server Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package flycheck)

(provide 'lsp-setup)

;;; lsp-setup.el ends here
