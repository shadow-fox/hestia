;;; init-modeline.el --- Modeline config

;;; Commentary:

;;; Code:

(use-package all-the-icons
  :straight t)

(use-package diminish
  :straight t)

(use-package moody
  :straight t
  ;;:after hestia-fonts                     ; we need its hook
  :config
  (moody-replace-mode-line-buffer-identification)

  ;; TODO: this should be part of a future `hestia-simple.el' or something
  (defun hestia/moody--even-p (n)
    (if (numberp n)
        (= (% n 2) 0)
      (user-error "%s is not a number" n)))

  (defun hestia/moody--height ()
    "Set Moody height to an even number.
Bind this to a hook that gets called after loading/changing the
mode line's typeface (or the default one if they are the same)."
    (let* ((font (face-font 'mode-line))
           (height (truncate (* 1.35 (aref (font-info font) 2))))
           (height-even (if (hestia/moody--even-p height) height (+ height 1))))
      (if font
          height-even
        20)))

  (defun hestia/moody--mode-line-height ()
    "Set Moody height to the value of `hestia/moody--height'."
    (setq moody-mode-line-height (funcall 'hestia/moody--height)))
  :hook (hestia-fonts-set-typeface-hook . hestia/moody--mode-line-height))

(provide 'init-modeline)
;;; init-modeline.el ends here
