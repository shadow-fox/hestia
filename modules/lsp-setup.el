;;; lsp-setup.el --- LSP Server Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package emacs
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package editorconfig)

(provide 'lsp-setup)

;;; lsp-setup.el ends here
