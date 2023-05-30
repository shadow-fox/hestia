;;; init.el --- Init File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:



(defvar hestia-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Emacs is starting up... Be patient, Master %s!" hestia-user)

;; Define Hestia's directory structure
(defvar hestia-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Hestia distribution.")

(defvar hestia-core-dir (expand-file-name "core/" hestia-dir)
  "The home of Hestia's core functionality.")

(defvar hestia-modules-dir (expand-file-name  "modules/" hestia-dir)
  "This directory houses all of the built-in Hestia modules.")

(defvar hestia-editor-dir (expand-file-name  "editor/" hestia-dir)
  "This directory houses all of the built-in Hestia editor.")

(defvar hestia-local-dir (expand-file-name ".local/" hestia-dir)
  "Hidden dir to store other files that are not part of configuration.")

(defvar hestia-savefile-dir (expand-file-name "savefile" hestia-local-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p hestia-local-dir)
  (make-directory hestia-local-dir))

(unless (file-exists-p hestia-savefile-dir)
  (make-directory hestia-savefile-dir))

(add-to-list 'load-path hestia-core-dir)
(add-to-list 'load-path hestia-modules-dir)
(add-to-list 'load-path hestia-editor-dir)

;; Disable the damn thing by making it disposable.
(setq custom-file (concat hestia-dir "custom.el"))

(require 'init-core)
(require 'init-editor)
(require 'init-modules)


;;; init.el ends here
