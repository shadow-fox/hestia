;;; lang-java.el --- Summary

;;; Commentary:

;;; Code:

(require 'init-prog)

(add-hook 'java-mode-hook #'rainbow-delimiters-mode)

;; (use-package meghanada
;;   :straight t
;;   :hook (java-mode-hook . meghanada-mode)
;;   :init
;;   (setq meghanada-server-install-dir (concat hestia-local-dir "/meghanada-server/")
;;         meghanada-use-company t
;;         meghanada-use-flycheck t
;;         meghanada-use-eldoc t
;;         meghanada-use-auto-start t))

(use-package groovy-mode
  :straight t)

(use-package lsp-java
  :after lsp-mode
  :straight t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-to-list 'lsp-enabled-clients 'jdtls))
  ;; (progn
  ;;   (setq
  ;;    lsp-java-java-path "/usr/lib/jvm/java-15-openjdk/bin/java"
  ;;    lsp-java-server-install-dir hestia-local-dir
  ;;    lsp-java-workspace-cache-dir t
  ;;    lsp-java-format-enabled t
  ;;    lsp-java-format-comments-enabled t
  ;;    lsp-java-save-action-organize-imports t
  ;;    lsp-java-save-action-organize-imports t
  ;;    lsp-java-import-gradle-enabled t
  ;;    lsp-java-import-maven-enabled t
  ;;    lsp-java-auto-build t
  ;;    lsp-print-io t
  ;;    lsp-java-progress-report t
  ;;    lsp-java-completion-guess-arguments t
  ;;    lsp-java-enable-file-watch t

  ;;    ;; Debug Server
  ;;    lsp-java-trace-server t
  ;;    lsp-java-progress-report t
  ;;    )
  ;;   )
;;)

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java
  :straight nil)

(provide 'lang-java)
;;; lang-java.el ends here
