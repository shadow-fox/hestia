;;; lang-cpp.el --- Summary

;;; Commentary:

;;; Code:

(require 'init-prog)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(use-package cmake-ide
  :straight t)


(provide 'lang-cpp)
;;; lang-cpp.el ends here
