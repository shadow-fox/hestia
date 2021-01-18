;;; init-fonts.el --- Font configurations for my dotemacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package unicode-fonts
  :straight t
  :config
  (unicode-fonts-setup))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq read-quoted-char-radix 16)

(provide 'init-fonts)
