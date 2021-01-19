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
  (setq which-key-max-display-columns nil)
  (setq which-key-separator "  ")
  (setq which-key-special-keys nil)
  (setq which-key-paging-key "<next>")
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package helpful
  :straight t
  :config
  (setq apropos-do-all t))

(general-define-key
 [remap describe-function]  'helpful-callable
 [remap describe-command]   'helpful-command
 [remap describe-variable]  'helpful-variable
 [remap describe-key]       'helpful-key
 [remap describe-symbol]    'helpful-symbol)

(use-package server
  :straight (:type built-in)
  :when (display-graphic-p)
  :after pre-command-hook after-find-file focus-out-hook
  :defer 1
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :config
  (setq server-auth-dir (concat hestia-local-dir "/server/"))
  (unless (server-running-p)
    (server-start)))

(require 'init-security)
(require 'init-defaults)
(require 'init-formatting)
(require 'init-save-recent)
(require 'init-autorevert)
(require 'init-frames)

(provide 'init-core)
;;; init-core.el ends here
