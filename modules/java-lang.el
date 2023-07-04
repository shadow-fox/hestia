;;; java-lang.el --- Java config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :hook
  (java-mode . eglot-ensure)
  )


(provide 'java-lang)

;;; java-lang.el ends here
