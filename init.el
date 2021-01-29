;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Bootstrap config

(defvar hestia-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "[Hestia] Hestia is powering up... Be patient, Master %s!" hestia-user)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define Hestia's directory structure
(defvar hestia-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Hestia distribution.")

(defvar hestia-core-dir (expand-file-name "core" hestia-dir)
  "The home of Hestia's core functionality.")

(defvar hestia-modules-dir (expand-file-name  "modules" hestia-dir)
  "This directory houses all of the built-in Hestia modules.")

(defvar hestia-editor-dir (expand-file-name  "editor" hestia-dir)
  "This directory houses all of the built-in Hestia editor.")

(defvar hestia-local-dir (expand-file-name ".local" hestia-dir)
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

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(require 'init-core)
(require 'init-editor)
(require 'init-modules)

(provide 'init)
;;; init.el ends here
