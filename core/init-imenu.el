;;; init-imenu.el --- IMenu

;;; Commentary:

;;; Code:

(use-package imenu
  :config
  (setq imenu-use-markers t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-max-item-length 100)
  (setq imenu-use-popup-menu nil)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-space-replacement " ")
  (setq imenu-level-separator "/")

  (defun hestia/imenu-vertical ()
    "Use a vertical Icomplete layout for `imenu'.
Also configure the value of `orderless-matching-styles' to avoid
aggressive fuzzy-style matching for this particular command."
    (interactive)
    (let ((orderless-matching-styles    ; make sure to check `orderless'
           '(orderless-literal
             orderless-regexp
             orderless-prefixes)))
      (icomplete-vertical-do (:height (/ (frame-height) 4))
        (call-interactively 'imenu))))

  (defun hestia/imenu-recenter-pulse ()
    "Recent `imenu' position at the top with subtle feedback.
Add this to `imenu-after-jump-hook'."
    (let ((pulse-delay .05))
      (recenter 0)
      (hestia/pulse-line)))

  (defun hestia/imenu-show-entry ()
    "Reveal index at point after successful `imenu' execution.
To be used with `imenu-after-jump-hook'."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry)
      (org-reveal t))
     ((when hestia/outline-minor-mode
        (outline-show-entry)))))

  :hook ((imenu-after-jump-hook . hestia/imenu-recenter-pulse)
         (imenu-after-jump-hook . hestia/imenu-show-entry)))

(use-package imenu-list
  :straight t
  :after imenu
  :config
  (defun hestia/imenu-list-dwim (&optional arg)
    "Convenience wrapper for `imenu-list'.
Move between the current buffer and a dedicated window with the
contents of `imenu'.

The dedicated window is created if it does not exist, while it is
updated once it is focused again through this command.

With \\[universal-argument] toggle the display of the window."
    (interactive "P")
    (if arg
        (imenu-list-smart-toggle)
      (with-current-buffer
          (if (eq major-mode 'imenu-list-major-mode)
              (pop-to-buffer (other-buffer (current-buffer) t))
            (imenu-list))))))

(use-package flimenu
  :straight t
  :after imenu
  :config
  (flimenu-global-mode 1))

(general-define-key
 "C-." 'hestia/imenu-vertical
 "C-," 'hestia/imenu-list-dwim)

(provide 'init-imenu)
;;; init-imenu.el ends here
