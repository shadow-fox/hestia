;;; init-buffer.el --- Buffer Config

;;; Commentary:

;;; Code:

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)

  (defun hestia/buffers-major-mode (&optional arg)
    "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion."
    (interactive "P")
    (let* ((major major-mode)
           (prompt "Buffers for ")
           (mode-string (format "%s" major))
           (mode-string-pretty (propertize mode-string 'face 'success)))
      (if arg
          (ibuffer t (concat "*" prompt mode-string "*")
                   (list (cons 'used-mode major)))
        (switch-to-buffer
         (read-buffer
          (concat prompt mode-string-pretty ": ") nil t
          (lambda (pair) ; pair is (name-string . buffer-object)
            (with-current-buffer (cdr pair) (derived-mode-p major))))))))

  (defun hestia/buffers-vc-root (&optional arg)
    "Select buffers that match the present `vc-root-dir'.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly.  Else use standard completion.

When no VC root is available, use standard `switch-to-buffer'."
    (interactive "P")
    (let* ((root (vc-root-dir))
           (prompt "Buffers for VC ")
           (vc-string (format "%s" root))
           (vc-string-pretty (propertize vc-string 'face 'success)))
      (if root
          (if arg
              (ibuffer t (concat "*" prompt vc-string "*")
                       (list (cons 'filename (expand-file-name root))))
            (switch-to-buffer
             (read-buffer
              (concat prompt vc-string-pretty ": ") nil t
              (lambda (pair) ; pair is (name-string . buffer-object)
                (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
        (call-interactively 'switch-to-buffer))))

  :hook (ibuffer-mode-hook . hl-line-mode))

(use-package ibuffer-vc
  :straight t
  :after (ibuffer vc))

(general-define-key
 "M-s b" 'hestia/buffers-major-mode
 "M-s v" 'hestia/buffers-vc-root
 "C-x C-b" 'ibuffer)

(general-define-key
 :keymaps 'ibuffer-mode-map
 "* f" 'ibuffer-mark-by-file-name-regexp
 "* g" 'ibuffer-mark-by-content-regexp ; "g" is for "grep"
 "* n" 'ibuffer-mark-by-name-regexp
 "s n" 'ibuffer-do-sort-by-alphabetic  ; "sort name" mnemonic
 "/ g" 'ibuffer-filter-by-content
 "/ V" 'ibuffer-vc-set-filter-groups-by-vc-root
 "/ <deletechar>" 'ibuffer-clear-filter-groups)

(provide 'init-buffer)
;;; init-buffer.el ends here
