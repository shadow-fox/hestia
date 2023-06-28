;;; hestia-prog.el --- Prog Mode File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Line numbers

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(use-package emacs
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'lisp-mode-hook #'display-line-numbers-mode))

(use-package magit)

(provide 'hestia-prog)

;;; hestia-prog.el ends here
