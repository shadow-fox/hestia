;;; complete.el --- Completions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Fuzzy & Narrowing
;; prescient.el , selectrum , Orderless , Snails
(use-package orderless
  :config
  (setq orderless-component-separator " +")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Minibuffer completion frameworks
;; vertico, mct, selectrum

(use-package vertico
  :init
  (vertico-mode))

;; completion at point UIs
;; corfu, company
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package cape
  :init
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-history cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

(use-package embark
  :general
  ("C-." 'embark-act)
  )

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  ;; orig. switch-to-buffer
  ("C-x b" 'consult-buffer)
  ;; orig. yank-pop
  ("M-y" 'consult-yank-pop)
  ;; orig. goto-line
  ("M-g g" 'consult-goto-line)
  ("M-g M-g" 'consult-goto-line)
  ("M-g i" 'consult-imenu)
  ("M-g I" 'consult-imenu-multi)
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :config
  (marginalia-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(provide 'complete)

;;; complete.el ends here
