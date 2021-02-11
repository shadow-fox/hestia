;;; lang-rust.el --- Summary:

;;; Commentary:

;;; Code:

(use-package rustic
  :straight t
  :defer t
  :config
  (add-to-list 'lsp-enabled-clients 'rust-analyzer))

(provide 'lang-rust)
;;; lang-rust.el ends here
