;;; lang-python.el --- Summary

;;; Commentary:

;;; Code:

(require 'init-prog)

(use-package poetry
  :straight t
  :after (python-mode)
  :hook
  (python-mode-hook . poetry-tracking-mode)
  ;;(python-mode-hook . lsp)
  :config
  (when (stringp (poetry-find-project-root))
    (poetry-venv-workon)))

(use-package python
  :straight (:type built-in)
  :hook
  (python-mode-hook . lsp-deferred)
  :config
  (add-to-list 'lsp-enabled-clients 'pyls))

;; (use-package lsp-jedi
;;   :straight t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))

;; (use-package lsp-pyright
;;   :straight t
;;   :hook
;;   (python-mode-hook . lsp)
;;   :config
;;   (add-to-list 'lsp-disabled-clients 'pyls)
;;   (add-to-list 'lsp-enabled-clients 'pyright)
;;   (setq lsp-pyright-venv-path 'poetry-get-virtualenv))

;; (use-package lsp-python-ms
;;   :straight t
;;   :init
;;   (add-to-list 'lsp-disabled-clients 'pyls)
;;   (add-to-list 'lsp-enabled-clients 'mspyls)
;;   (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode-hook . (lambda ()
;;                               (require 'lsp-python-ms)
;;                               (lsp))))

(provide 'lang-python)
;;; lang-python.el ends here
