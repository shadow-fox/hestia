;;; init-theme-font.el --- Summary

;;; Commentary:

;;; Code:

(set-face-attribute 'default nil :font "Iosevka" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 120 :weight 'regular)

;; (use-package dracula-theme
;;   :straight t
;;   :config
;;   (load-theme 'dracula t))

(use-package gruvbox-theme
  :straight t
  :init
  (load-theme 'gruvbox t))

(use-package all-the-icons
  :straight t)

(provide 'init-theme-font)
;;; init-theme-font.el ends here
