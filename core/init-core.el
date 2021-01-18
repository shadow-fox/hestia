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
  (setq use-package-hook-name-suffix nil))

;; provides `straight-x-clean-unused-repos' (part of `straight.el')
(use-package straight-x)

(use-package vc
  :config
  (setq vc-follow-symlinks t))

;; For Key-bindings.
(use-package general
  :straight t)

(use-package which-key
  :straight t
  :config
  (setq which-key-dont-use-unicode t)
  (setq which-key-add-column-padding 2)
  (setq which-key-show-early-on-C-h nil)
  ;;(setq which-key-idle-delay most-positive-fixnum)
  ;;(setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (setq which-key-show-prefix 'echo)
  (setq which-key-max-display-columns 3)
  (setq which-key-separator "  ")
  (setq which-key-special-keys nil)
  (setq which-key-paging-key "<next>")
  (which-key-mode))

(use-package helpful
  :straight t)

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please

(general-define-key
 "C-h f" 'helpful-callable
 "C-h v" 'helpful-variable
 "C-h F" 'helpful-function)

(provide 'init-core)
;;; init-core.el ends here
