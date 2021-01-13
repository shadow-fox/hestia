;;; init-pdf.el --- Summary:

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :straight t
  :config
  (pdf-tools-install))

(provide 'init-pdf)
;;; init-pdf.el ends here
