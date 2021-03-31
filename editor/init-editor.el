;;; init-editor.el --- Emacs as editor.

;;; Commentary:

;;; Code:

(use-package which-key
  :straight t
  :config
  (which-key-mode t))

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :straight t
  :hook (
         (prog-mode-hook . highlight-numbers-mode)
         (conf-mode-hook . highlight-numbers-mode))
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp.
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(use-package restart-emacs
  :commands (restart-emacs)
  :straight t)

(use-package general
  :straight t)

(use-package move-text
  :straight t)

(general-define-key
 "M-<up>" 'move-text-up
 "M-<down>" 'move-text-down)

(require 'init-theme-font)
(require 'init-selectrum)

(require 'init-projects)

(provide 'init-editor)
;;; init-editor.el ends here
