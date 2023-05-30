;;; hestia-editor.el --- Hestia Theme Font File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package modus-themes
  :straight t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

(set-face-attribute 'default nil :font "Fira Code" :height 150)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 150)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height 150 :weight 'regular)

(provide 'hestia-theme)

;;; hestia-theme.el ends here
