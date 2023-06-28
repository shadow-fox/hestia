;;; org-setup Org Mode Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq org-directory "/home/fox/storage/Documents/")

(setq org-id-locations-file (concat hestia-misc-dir "org-id-locations"))

(setq org-roam-directory "/home/fox/storage/Documents/roam-brain/")

(setq org-roam-db-location (concat hestia-misc-dir "/org-roam.db"))

;;(use-package denote)

(use-package org-roam
  :config
  (org-roam-db-autosync-mode))

;;(use-package org-appear)

(provide 'org-setup)

;;; org-setup.el ends here
