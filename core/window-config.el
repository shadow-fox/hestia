;;; window-config.el --- Window Management Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package desktop
  :config
    (setq desktop-dirname (concat hestia-misc-dir "/desktop"))
    (setq desktop-base-file-name (concat hestia-misc-dir "/autosave"))
    (setq desktop-base-lock-name (concat hestia-misc-dir "/autosave-lock"))
    (desktop-save-mode t)
  )


(provide 'window-config)

;;; window-config.el ends here
