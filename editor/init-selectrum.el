;;; init-selectrum.el --- Summary: -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode t))

(use-package selectrum
  :straight t
  :hook
  (after-init-hook . selectrum-mode)
  :config
  (setq-default selectrum-fix-vertical-window-height t))

(use-package selectrum-prescient
  :straight t
  :init
  (selectrum-prescient-mode t)
  :after (prescient selectrum)
  )

(defun refresh-selectrum ()
  (setq selectrum--previous-input-string nil))

(use-package embark
  :straight t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :hook
  (embark-pre-action-hook . refresh-selectrum)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setq embark-action-indicator
      (lambda (map &optional _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)
  (setq embark-quit-after-action nil))

(use-package marginalia
  :straight t
  :hook
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (after-init-hook .   marginalia-mode)
  :config
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected))))
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(use-package consult
  :straight t
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config

  ;; Optionally configure preview. Note that the preview-key can also be
  ;; configured on a per-command basis via `consult-config'. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from:
  ;; * projectile-project-root
  ;; * vc-root-dir
  ;; * project-roots
  ;; * locate-dominating-file
  ;; * project-x
  (autoload 'projectile-project-root "projectile")
  (setq-default consult-project-root-function #'projectile-project-root)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;; (setq consult-project-root-function
  ;;       (lambda () (locate-dominating-file "." ".git")))
  )

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode-hook . embark-consult-preview-minor-mode))

(use-package company-prescient
  :straight t
  :after (selectrum company-mode)
  :config
  (setq company-prescient-sort-length-enable nil)
  (company-prescient-mode t))

(general-define-key
 "C-S-a" 'embark-act
 [remap describe-bindings] 'embark-bindings)

(general-define-key
 "C-x b" 'consult-buffer
 "M-y"   'consult-yank-pop
 "M-s g" 'consult-grep
 "M-s G" 'consult-git-grep
 "M-s r" 'consult-ripgrep)

(provide 'init-selectrum)
;;; init-selectrum.el ends here
