;;; init-editor.el --- Emacs as editor.

;;; Commentary:

;;; Code:

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-default-init t)
;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
;; pull in a ton of packages.
(setq       initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq use-file-dialog nil)
(setq use-dialog-box t)
(setq inhibit-splash-screen t)
(setq initial-buffer-choice t)
(setq inhibit-startup-buffer-menu t)
(setq frame-title-format '("%b"))
(setq echo-keystrokes 0.25)
(setq default-input-method "greek")
(setq ring-bell-function 'ignore)
(setq visible-cursor nil)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)
;;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq widget-image-enable nil)

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

(setq default-frame-alist
      '((top . 200) (left . 600)
        (width . 200) (height . 65)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)
(blink-cursor-mode 0)

;; default file paths for emacs related stuff.

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(setq save-place-file (concat hestia-savefile-dir "/saveplace"))
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

(setq-default fill-column 100)


;;; Paren mode is part of the theme
(show-paren-mode t)


(use-package autorevert
  :diminish
  :config
  (setq auto-revert-verbose t)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

(use-package vc
  :config
  (setq vc-follow-symlinks t))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "savehist" hestia-local-dir))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (after-init-hook . savehist-mode))

(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package desktop
  :config
  (setq desktop-auto-save-timeout 300)
  (setq desktop-dirname hestia-local-dir)
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save nil)
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode 1))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

(use-package newcomment
  :config
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)

  (defun hestia/comment-dwim (&optional arg)
    "Alternative to `comment-dwim': offers a simple wrapper
around `comment-line' and `comment-dwim'.

If the region is active, then toggle the comment status of the
region or, if the major mode defines as much, of all the lines
implied by the region boundaries.

Else toggle the comment status of the line at point."
    (interactive "*P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg)))))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

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

(use-package pulse
  :config
  (defface hestia/pulse-line-modus-theme
    '((t :inherit modus-theme-subtle-green :extend t))
    "Ad-hoc face for `hestia/pulse-line'.
This is done because it is not possible to highlight empty lines
without the `:extend' property.")

  (defun hestia/pulse-line (&optional face)
    "Temporarily highlight the current line."
    (interactive)
    (let ((start (if (eobp)
                     (line-beginning-position 0)
                   (line-beginning-position)))
          (end (line-beginning-position 2))
          (pulse-delay .04)
          (face (or face 'hestia/pulse-line-modus-theme)))
      (pulse-momentary-highlight-region start end face))))

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode hestia/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if hestia/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local)))))

(use-package emacs
  :hook
  (after-init-hook . column-number-mode))

(use-package restart-emacs
  :straight t)

(require 'init-fonts)
(require 'init-themes)

(require 'init-flyspell)

(require 'init-window)
(require 'init-modeline)
(require 'init-scratch)
(require 'init-buffer)
(require 'init-frames)

;; C-c l is used for `org-store-link'.  The mnemonic for this is to
;; focus the Line and also works as a variant of C-l.
(general-define-key
 "C-;" 'hestia/comment-dwim
 "C-:" 'comment-kill
 "M-;" 'comment-indent
 "C-x C-;" 'comment-box
 "<s-escape>" 'hestia/pulse-line
 "C-c l" 'hestia/scroll-centre-cursor-mode)

(provide 'init-editor)
;;; init-editor.el ends here
