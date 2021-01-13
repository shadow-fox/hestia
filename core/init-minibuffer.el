;;; init-minibuffer.el --- Minibuffer

;;; Commentary:

;;; Code:

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless))
  :config
  (icomplete-mode t))

(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark" :branch "master")
  :bind
  ("C-S-a" . embark-act)               ; pick some comfortable binding
  :config
  ;; For Selectrum users:
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))


(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  ;; :bind (;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
  ;;        :map embark-general-map
  ;;        ("A" . marginalia-cycle)
  ;;        )

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

;; Example configuration for Consult
(use-package consult
  :straight (consult :type git :host github :repo "minad/consult" :branch "main")
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure register preview function.
  ;; This gives a consistent display for both `consult-register' and
  ;; the register preview when editing registers.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Configure preview. Note that the preview-key can also be configured on a
  ;; per-command basis via `consult-config'.
  ;; The default value is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :straight (consult :type git :host github :repo "minad/consult" :branch "main")
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))


;; Space should never complete: use
;; it for `orderless' groups.
(general-define-key
 :keymaps 'minibuffer-local-completion-map
 "SPC" 'nil)

(general-define-key          
 :keymaps 'minibuffer-local-completion-map
 "<return>" 'minibuffer-force-complete-and-exit ; exit with completion
 "C-j" 'exit-minibuffer)      ; force input unconditionally

(general-define-key
 :keymaps 'completion-list-mode-map
 "h" 'hestia/describe-symbol-at-point
 "w" 'hestia/completions-kill-symbol-at-point
 "i" 'hestia/completions-insert-symbol-at-point
 "j" 'hestia/completions-insert-symbol-at-point-exit
 "n" 'next-line
 "p" 'previous-line
 "f" 'next-completion
 "b" 'previous-completion
 "M-v" 'hestia/focus-minibuffer)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
