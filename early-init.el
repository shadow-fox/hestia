;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable these graphical options. As of now I am not using these.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

(setq use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      make-backup-files nil
      backup-inhibited nil ; Not sure if needed, given `make-backup-files'
      create-lockfiles nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;;; early-init.el ends here
