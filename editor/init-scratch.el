;;; init-scratch.el --- Scratch buffer.

;;; Commentary:

;;; Code:

(use-package scratch
  :straight t
  :config
  (defun hestia/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode)
           (string (format "Scratch buffer for: %s\n\n" mode))
           (region (with-current-buffer (current-buffer)
                     (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))
                     ""))
           (text (concat string region)))
      (when scratch-buffer
        (save-excursion
          (insert text)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  :hook (scratch-create-buffer-hook . hestia/scratch-buffer-setup))

(general-define-key
 "C-c s" 'scratch)

(provide 'init-scratch)
;;; init-scratch.el ends here
