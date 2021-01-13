;;; init-flyspell.el --- Flyspell with aspell

;;; Commentary:

;;; Code:

(use-package dictionary
  :straight t)

(use-package flyspell
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")

  (defvar hestia/ispell-dictionaries
    '(("English" . "en"))
    "Alist of dictionaries I may use.
It is used by `hestia/ispell-dictionaries-complete'.")

  (defun hestia/ispell-dictionaries-complete ()
    "Select an item from `hestia/ispell-dictionaries'."
    (interactive)
    (let* ((dicts (mapcar #'car hestia/ispell-dictionaries))
           (choice (completing-read "Select dictionary: " dicts nil t))
           (key (cdr (assoc `,choice hestia/ispell-dictionaries))))
      (ispell-change-dictionary key)
      (message "Switched to %s" key)))

  (defun hestia/flyspell-dwim ()
    "Spell check region or select dictionary.

Use `flyspell-region' on the active region, else invoke
`hestia/ispell-dictionaries-complete'."
    (interactive)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if (use-region-p)
          (flyspell-region beg end)
        (hestia/ispell-dictionaries-complete)))))

(general-define-key
 "M-$" 'hestia/flyspell-dwim
 "C-M-$" 'hestia/ispell-dictionaries-complete)

(general-define-key
 :keymaps 'flyspell-mode-map
 "C-;" 'nil)

(provide 'init-flyspell)
;;; init-flyspell.el ends  here
