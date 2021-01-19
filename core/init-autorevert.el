;;; init-autorevert.el --- Summary:

;;; Commentary:

;;; Code:


(use-package autorevert
  :straight (:type built-in)
  ;; revert buffers when their files/state have changed
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
  :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  :hook (doom-switch-window . doom-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, abuse the
  ;; heck out of inotify handles _or_ aggresively poll your buffer list every X
  ;; seconds. Too many inotify handles can grind Emacs to a halt if you preform
  ;; expensive or batch processes on files outside of Emacs (e.g. their mtime
  ;; changes), and polling your buffer list is terribly inefficient as your
  ;; buffer list grows into the tens or hundreds.
  ;;
  ;; So Doom uses a different strategy: we lazily auto revert buffers when the
  ;; user a) saves a file, b) switches to a buffer (or its window), or c) you
  ;; focus Emacs (after using another program). This way, Emacs only ever has to
  ;; operate on, at minimum, a single buffer and, at maximum, X buffers, where X
  ;; is the number of open windows (which is rarely, if ever, over 10).
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (doom-visible-buffers))
      (with-current-buffer buf
        (doom-auto-revert-buffer-h)))))

(provide 'init-autorevert)
;;; init-autorevert.el ends here
