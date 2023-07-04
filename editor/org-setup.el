;;; org-setup Org Mode Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq org-directory "/home/fox/storage/Documents/")

(setq org-id-locations-file (concat hestia-misc-dir "org-id-locations"))

(setq org-roam-directory "/home/fox/storage/Documents/roam-brain/")

(setq org-roam-db-location (concat hestia-misc-dir "/org-roam.db"))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(use-package org-roam
  :config
  (org-roam-db-autosync-mode))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(provide 'org-setup)

;;; org-setup.el ends here
