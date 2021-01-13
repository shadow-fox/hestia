;;; init-emacs.el --- Summary:

;;; Commentary:

;;; Code:

;; For Key-bindings.
(use-package general
  :straight t)

(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)               ; only for mouse events
  (setq inhibit-splash-screen t)
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)))

(use-package emacs
  :config
  (setq frame-title-format '("%b"))
  (setq echo-keystrokes 0.25)
  (setq default-input-method "greek")
  (setq ring-bell-function 'ignore)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'overwrite-mode 'disabled t))

(use-package emacs
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package emacs
  :config
  (setq backup-directory-alist
        '(("." . "~/.emacs.d/.local/backups/")))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq create-lockfiles nil))

(use-package saveplace
  :config
  (setq save-place-file (concat "savefile" hestia-local-dir))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/.local/savehist")
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (after-init-hook . savehist-mode))

(use-package restart-emacs
  :straight t)

;; (use-package server
;;   :hook (after-init-hook . server-start))

(use-package emacs
  :init
  (setq initial-buffer-choice t)
  (setq inhibit-startup-echo-area-message "hestia")
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-buffer-menu t))

(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package emacs
  :config
  (setq save-interprogram-paste-before-kill t))

(use-package autorevert
  :diminish
  :config
  (setq auto-revert-verbose t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

(use-package tooltip
  :config
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  :hook (after-init-hook . tooltip-mode))

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-l" . prot/scroll-centre-cursor-mode))

(use-package emacs
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package vc
  :config
  (setq vc-follow-symlinks t))

(provide 'init-emacs)
;;; init-emacs.el ends here
