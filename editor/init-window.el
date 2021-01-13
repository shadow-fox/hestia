;;; init-window.el --- Windows

;;; Commentary:

;;; Code:

(use-package emacs
  :config
  (defvar hestia/window-configuration nil
    "Current window configuration.
Intended for use by `hestia/window-monocle'.")

  (define-minor-mode hestia/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when hestia/window-configuration
          (set-window-configuration hestia/window-configuration))
      (setq hestia/window-configuration (current-window-configuration))
      (delete-other-windows)))
  :bind ("s-m" . hestia/window-single-toggle))

(use-package window
  :init
  (setq display-buffer-alist
        '(;; top side window
          ("\\*Bongo-Elfeed Queue.*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . -2))
          ("\\*\\(elfeed-mpv-output\\|world-clock\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . -1))
          ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*Messages.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ;; bottom side window
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("^\\(\\*e?shell\\|vterm\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
           (side . left)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; right side window
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0)
           (window-parameters
            . ((no-other-window . t)
               (mode-line-format
                . (" "
                   mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode)))

;; These are all experimental.  Just showcasing the power of passing
;; parameters to windows or frames.
(use-package emacs
  :commands (hestia/window-dired-vc-root-left
             hestia/make-frame-floating-with-current-buffer
             hestia/display-buffer-at-bottom)
  :config
  (defun hestia/window-dired-vc-root-left ()
    "Open project or dir `dired' in a side window.

NOTE: For demo purposes."
    (interactive)
    (let ((dir (if (eq (vc-root-dir) nil)
                   (dired-noselect default-directory)
                 (dired-noselect (vc-root-dir)))))
      (display-buffer-in-side-window
       dir `((side . left)
             (slot . -1)
             (window-width . 0.16)
             (window-parameters
              . ((no-other-window . t)
                 (no-delete-other-windows . t)
                 (mode-line-format
                  . (" "
                     mode-line-buffer-identification))))))
      (with-current-buffer dir
        (rename-buffer "*Dired-Side*")
        (setq-local window-size-fixed 'width)))
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (add-to-list 'aw-ignored-buffers "*Dired-Side*"))))

  (defun hestia/make-frame-floating-with-current-buffer ()
    "Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of my tiling
window manager (BSPWM) to present the frame in a floating state.

NOTE: For demo purposes."
    (interactive)
    (make-frame '((name . "my_float_window")
                  (window-system . x)
                  (minibuffer . nil))))

  (defun hestia/display-buffer-at-bottom ()
    "Move the current buffer to the bottom of the frame.
This is useful to take a buffer out of a side window.

NOTE: For demo purposes."
    (interactive)
    (let ((buffer (current-buffer)))
      (with-current-buffer buffer
        (delete-window)
        (display-buffer-at-bottom
         buffer
         `((window-parameters
            . ((mode-line-format
                . (" "
                   mode-line-buffer-identification))))))))))

(use-package winner
  :hook (after-init-hook . winner-mode))

(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode -1)
  (tab-bar-history-mode -1)

  (defun hestia/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t)))))))

;; This is only included as a reference.
(use-package tab-line
  :disabled
  :commands (tab-line-mode global-tab-line-mode)
  :config
  (global-tab-line-mode -1))

(use-package windmove
  :config
  (setq windmove-create-window nil))

(use-package transpose-frame
  :straight t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
)

(general-define-key
 "s-n" 'next-buffer
 "s-p" 'previous-buffer
 "s-o" 'other-window
 "s-2" 'split-window-below
 "s-3" 'split-window-right
 "s-0" 'delete-window
 "s-1" 'delete-other-windows
 "s-5" 'delete-frame
 "C-x _" 'balance-windows
 "C-x +" 'balance-windows-area
 "s-q" 'window-toggle-side-windows
 "<s-right>" 'winner-redo
 "<s-left>" 'winner-undo
 "C-s-t" 'flop-frame ; what I consider "transpose" in this context
 "C-s-r" 'rotate-frame-clockwise
 "C-s-k" 'windmove-up
 "C-s-l" 'windmove-right
 "C-s-j" 'windmove-down
 "C-s-h" 'windmove-left
 ;; numpad keys clockwise: 8 6 2 4
 "<kp-up>" 'windmove-up
 "<kp-right>" 'windmove-right
 "<kp-down>" 'windmove-down
 "<kp-left>" 'windmove-left
 "C-x t t" 'hestia/tab-bar-select-tab-dwim
 "s-t" 'hestia/tab-bar-select-tab-dwim
 "<s-tab>" 'tab-next
 "<S-s-iso-lefttab>" 'tab-previous)

(provide 'init-window)
;;; init-window.el ends here
