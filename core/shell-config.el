;;; shell-config.el --- App related config  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package vterm)

(provide 'shell-config)

;;; shell-config.el ends here
