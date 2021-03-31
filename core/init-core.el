;;; init-core.el --- Emacs Core.

;;; Commentary:

;;; Code:

(setq straight-base-dir hestia-local-dir)

(setq straight-use-package-by-default nil)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" hestia-local-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)  ; ESSENTIAL for `straight.el'
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil)
  (setq use-package-verbose t))

;; provides `straight-x-clean-unused-repos' (part of `straight.el')
(use-package straight-x)

(require 'init-defaults)

(provide 'init-core)
;;; init-core.el ends here
