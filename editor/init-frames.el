;;; init-frames.el -- Summary

;;; Commentary:

;;; Code:

(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-reverse-frame-list t)
  (setq-default cursor-in-non-selected-windows 'hollow)
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "deep sky blue"
   :weight 'bold
   :height 3.0)
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "lawn green"))

(general-define-key
 "C-x o" 'ace-window)

;; (use-package workgroups2
;;   :straight t
;;   :config
;;   (setq wg-session-file (concat hestia-local-dir "/emacs_workgroups"))
;;   (setq wg-prefix-key (kbd "C-c w"))
;;   ;; What to do on Emacs exit / workgroups-mode exit?
;;   ;;(setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
;;   ;;(setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
;;   ;; Mode Line changes
;;   ;; Display workgroups in Mode Line?
;;   (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
;;   (setq wg-flag-modified t)                 ; Display modified flags as well
;;   (setq wg-mode-line-decor-left-brace "["
;; 	wg-mode-line-decor-right-brace "]"  ; how to surround it
;; 	wg-mode-line-decor-divider ":")
;;   (workgroups-mode t))
;; 
;; (general-define-key
;;  "<pause>" 'wg-reload-session
;;  "C-S-<pause>" 'wg-save-session
;;  "C-c w s" 'wg-switch-to-workgroup)

(provide 'init-frames)
;;; init-frames.el ends here
