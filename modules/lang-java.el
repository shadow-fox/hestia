;;; lang-java.el --- Summary

;;; Commentary:

;;; Code:

(require 'init-prog)

(use-package meghanada
  :straight t
  :hook (java-mode-hook . meghanada-mode)
  :init
  (setq meghanada-server-install-dir (concat hestia-local-dir "meghanada-server/")
        meghanada-use-company t
        meghanada-use-flycheck t
        meghanada-use-eldoc t
        meghanada-use-auto-start t))

(provide 'lang-java)
;;; lang-java.el ends here
