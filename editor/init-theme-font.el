;;; init-theme-font.el --- Summary

;;; Commentary:

;;; Code:

(set-face-attribute 'default nil :font "Fira Code" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height 120 :weight 'regular)

(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t))

(use-package all-the-icons
  :straight t
  :init
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)))

;;(use-package solaire-mode
;;  :straight t
;;  :config
;;  (solaire-global-mode +1)
;;  (solaire-mode-swap-bg))

(provide 'init-theme-font)
;;; init-theme-font.el ends here
