;;; init-completions.el --- Summary

;;; Commentary:

;;; Code:

(use-package company
  :straight t
  :hook
  (after-init-hook . global-company-mode))

(use-package company-quickhelp
  :straight t
  :after (company)
  :hook
  (after-init-hook . company-quickhelp-mode))

;; (general-define-key
;;  :keymaps 'company-active-map
;;  "M-n" nil
;;  "M-p" nil
;;  "C-n" 'company-select-next
;;  "C-p" 'company-select-previous)

;; (general-define-key
;;  :keymaps 'company-mode-map
;;  [remap completion-at-point] 'company-complete
;;  [remap indent-for-tab-command] 'company-indent-or-complete-common)

(provide 'init-completions)
;;; init-completions.el ends here
