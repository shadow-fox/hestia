;;; init-electric.el --- Electric Mode

;;; Commentary:
;; Emacs labels as “electric” any behaviour that involves
;; contextual auto-insertion of characters.

;; Get the cryptic numbers via
;; (string-to-char "\t")

;;; Code:

(use-package electric
  :config
  (setq electric-pair-inhibit-predicate'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars
        '(9
          10
          32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  :hook (after-init-hook . (lambda ()
                             (electric-indent-mode 1)
                             (electric-pair-mode -1)
                             (electric-quote-mode -1))))

(use-package smartparens
  :straight t)

(provide 'init-electric)
;;; init-electric.el ends here
