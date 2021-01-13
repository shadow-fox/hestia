;;; init-completions.el --- Summary

;;; Commentary:

;;; Code:

(use-package company
  :straight t
  :hook
  (after-init-hook . global-company-mode)
  :config
  (setq company-idle-delay 0.125)
  (setq company-transfers '(company-sort-by-occurrence))
  (setq company-minimum-prefix-length 1))

(use-package company-quickhelp
  :straight t
  :hook
  (after-init-hook . company-quickhelp-mode))

(general-define-key
 :keymaps 'company-active-map
 "M-n" nil
 "M-p" nil
 "C-n" 'company-select-next
 "C-p" 'company-select-previous)

(provide 'init-completions)
;;; init-completions.el ends here
