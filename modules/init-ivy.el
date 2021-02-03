;;; init-ivy.el --- Summary

;;; Commentary:

;;; Code:

(use-package ivy
  :straight t
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)

  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system 'ivy
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)
  (setq swiper-action-recenter t)
  (ivy-mode t))

(use-package ivy-hydra
  :straight t)

(use-package ivy-avy
  :straight t
  :after ivy)

(use-package counsel
  :straight t
  :config
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)

  ;; REVIEW Counsel allows `counsel-rg-base-command' to be a string or list.
  ;;        This backwards compatibility complicates things for Doom. Simpler to
  ;;        just force it to always be a list.
  (when (stringp counsel-rg-base-command)
    (setq counsel-rg-base-command (split-string counsel-rg-base-command)))
  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
  ;; `counsel-imenu' -- no sorting for imenu. Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))
  ;; `counsel-find-file'
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (counsel-mode t))

(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)
  (ivy-rich-mode t))

(use-package ivy-prescient
  :straight t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package wgrep
  :straight t
  :config (setq wgrep-auto-save-buffer t))

(use-package all-the-icons-ivy
  :straight t
  :after ivy
  :config
  ;; `all-the-icons-ivy' is incompatible with ivy-rich's switch-buffer
  ;; modifications, so we disable them and merge them ourselves
  (setq all-the-icons-ivy-buffer-commands nil)

  (all-the-icons-ivy-setup))

(use-package amx
  :straight t
  :config
  (setq amx-save-file (concat hestia-local-dir "/amx-items")))

;; (general-define-key
;;  "C-c s s" 'swiper-isearch
;;  "M-x" 'counsel-M-x
;;  "C-x C-f" 'counsel-find-file
;;  "C-c s r" 'counsel-rg
;;  "C-c c" 'counsel-compile)

(provide 'init-ivy)
;;; init-ivy.el ends here
