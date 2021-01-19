;;; init-save-recent.el --- Summary:

;;; Commentary:

;;; Code:

(use-package recentf
  :straight (:type built-in)
  ;; Keep track of recently opened files
  :commands easymenu tree-widget timer
  :hook (doom-first-file . recentf-mode)
  :commands recentf-open-files
  :config
  (setq recentf-filename-handlers
        '(;; Text properties inflate the size of recentf's files, and there is
          ;; no purpose in persisting them, so we strip them out.
          substring-no-properties
          ;; Resolve symlinks of local files. Otherwise we get duplicate
          ;; entries opening symlinks.
          doom--recent-file-truename
          ;; Replace $HOME with ~, which is more portable, and reduces how much
          ;; horizontal space the recentf listing uses to list recent files.
          abbreviate-file-name)
        recentf-save-file (concat hestia-local-dir "/recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))


(use-package savehist
  :straight (:type built-in)
  ;; persist variables across sessions
  :defer custom
  :hook (doom-first-input . savehist-mode)
  :init
  (setq savehist-file (concat hestia-local-dir "/savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  )

(use-package saveplace
  :straight (:type built-in)
  ;; persistent point location in buffers
  :hook (doom-first-file . save-place-mode)
  :init
  (setq save-place-file (concat hestia-local-dir "/saveplace")
        save-place-limit 100))

(provide 'init-save-recent)
;;; init-save-recent.el ends here
