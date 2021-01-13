
;; (use-package modus-operandi-theme
;;   :straight t)

;; (use-package modus-vivendi-theme
;;   :straight t)

(use-package modus-themes
  :straight (:host gitlab :repo "protesilaos/modus-themes" :branch "main")
;;  :load-path (concat hestia-local-dir "/straight/repos/modus-themes")      ; Custom path due to my dev needs
  :init
  (setq custom-safe-themes t)           ; Due to my dev needs
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-fringes nil ; {nil,'subtle,'intense}
        modus-themes-mode-line 'moody ; {nil,'3d,'moody}
        modus-themes-syntax nil ; {nil,'faint,'yellow-comments,'green-strings,'yellow-comments-green-strings,'alt-syntax,'alt-syntax-yellow-comments}
        modus-themes-intense-hl-line nil
        modus-themes-intense-paren-match nil
        modus-themes-links 'neutral-underline ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
        modus-themes-no-mixed-fonts nil
        modus-themes-prompts 'subtle ; {nil,'subtle,'intense}
        modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}
        modus-themes-diffs 'fg-only ; {nil,'desaturated,'fg-only}
        modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
        modus-themes-headings  ; Read the manual for this one
        '((1 . section)
          (t . line))
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings nil
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)
  :config
  ;;(use-package modus-operandi-theme)
  ;;(use-package modus-vivendi-theme)
  :hook (after-init-hook . modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package emacs
  :commands (wcag clr)
  :config
  (defun wcag (hex)
    (apply #'+
           (cl-mapcar
            (lambda (k x)
              (* k (if (<= x 0.03928)
                       (/ x 12.92)
                     (expt (/ (+ x 0.055) 1.055) 2.4))))
            '(0.2126 0.7152 0.0722)
            (color-name-to-rgb hex))))

  (defun clr (c1 c2)
    (let ((ct (/ (+ (wcag c1) 0.05)
                 (+ (wcag c2) 0.05))))
      (max ct (/ ct)))))

(provide 'init-themes)
