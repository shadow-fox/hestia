;;; init-replace.el --- Occur & Replace

;;; Commentary:

;;; Code:

(use-package replace
  :config
  (setq list-matching-lines-jump-to-current-line t)
  :hook ((occur-mode-hook . hl-line-mode)
         (occur-mode-hook . (lambda ()
                              (toggle-truncate-lines t)))))
(use-package re-builder
  :config
  (setq reb-re-syntax 'read))

(use-package visual-regexp
  :straight t
  :config
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

(use-package wgrep
  :straight t
  :commands wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(general-define-key
 "M-s u" 'hestia/occur-visit-or-list-urls
 "M-s M-o" 'multi-occur)

(general-define-key
 :keymaps 'occur-mode-map
 "t" 'toggle-truncate-lines)

(general-define-key
 "M-s g" 'hestia/rg-vc-or-dir
 "M-s r" 'hestia/rg-ref-in-dir)

(general-define-key
 :keymaps 'grep-mode-map
 "e" 'wgrep-change-to-wgrep-mode
 "C-x C-q" 'wgrep-change-to-wgrep-mode
 "C-c C-c" 'wgrep-finish-edit)

(provide 'init-replace)
;;; init-replace.el ends here
