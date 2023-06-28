;;; hestia-editor.el --- Hestia Theme Font File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ef-themes
  :config
  (load-theme 'ef-cherie :no-confirm))


(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))


(provide 'hestia-theme)

;;; hestia-theme.el ends here
