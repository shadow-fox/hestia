;;; lang-rust.el --- Summary:

;;; Commentary:

;;; Code:

(require 'init-prog)

(use-package rustic
  :straight t
  :demand t
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-command '("/usr/bin/rust-analyzer")))

(use-package lsp-rust
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (add-to-list 'lsp-enabled-clients 'rust-analyzer)
  :hook
  (rustic-mode-hook . lsp))

(provide 'lang-rust)
;;; lang-rust.el ends here
