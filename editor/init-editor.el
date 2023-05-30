;;; init-editor.el --- Init Editor File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package restart-emacs)

(require 'hestia-theme)
(require 'editor-defaults)
(require 'keymaps-defaults)
(require 'complete)

(require 'lsp-setup)

;; Language setup
(require 'rust-lang)

(provide 'init-editor)

;;; init-editor.el ends here
