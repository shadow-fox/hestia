;;; init-templates.el --- Summary:

;;; Commentary:

;;; Code:

(use-package yasnippet
  :straight t
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode)
  :config
  (yas-reload-all)
  :hook
  (prog-mode-hook . yas-minor-mode)
  (text-mode-hook . yas-minor-mode)
  (conf-mode-hook . yas-minor-mode)
  (snippet-mode-hook . yas-minor-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package auto-yasnippet
  :straight t
  :after yasnippet)

(provide 'init-templates)
;;; init-templates.el ends here
