;;; init-workspace.el --- Summary:

;;; Commentary:

;;; Code:

(use-package perspective
  :straight t
  :config
  (persp-mode t))

(use-package ace-window
  :defer t
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(use-package olivetti
  :init
  (olivetti-mode t)
  :straight t
  :config
  (setq olivetti-body-width 120))

(use-package popper
  :straight t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Flycheck errors\\*"
          help-mode
          compilation-mode))
  (popper-mode t))

(general-define-key
 "M-o" 'ace-window)

(general-define-key
 "C-c F" 'olivetti-mode)

(general-define-key
 "C-`"    'popper-toggle-latest
 "M-`"    'popper-cycle
 "C-M-`"  'popper-toggle-type)

(provide 'init-workspace)
;;; init-workspace.el ends here
