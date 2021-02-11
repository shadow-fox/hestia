;;; init-theme-font.el --- Summary

;;; Commentary:

;;; Code:

(set-face-attribute 'default nil :font "Iosevka" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 120 :weight 'regular)

(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t))

;; (use-package gruvbox-theme
;;   :straight t
;;   :init
;;   (load-theme 'gruvbox t))

(use-package all-the-icons
  :straight t
  :init
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-acario-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(provide 'init-theme-font)
;;; init-theme-font.el ends here
