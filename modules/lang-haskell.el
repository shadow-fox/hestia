;;; lang-haskell.el --- Summary

;;; Commentary:

;;; Code:

(require 'init-prog)

(use-package haskell-mode
  :straight t
  :hook
  (
   ;;(haskell-mode-hook . turn-on-haskell-unicode-input-method)
   (haskell-mode-hook . haskell-auto-insert-module-template)
   (haskell-mode-hook . haskell-decl-scan-mode)
   (haskell-mode-hook . flyspell-prog-mode)
   )
  :config
  (setq haskell-compile-cabal-build-command "cabal build --ghc-option=-ferror-spans")
  (setq haskell-tags-on-save t)

  (use-package haskell-interactive-mode
    :hook
    (haskell-mode-hook . interactive-haskell-mode)
    :config
    (setq haskell-process-suggest-remove-import-lines t)
    (setq haskell-process-auto-import-loaded-modules t)
    (setq haskell-process-log t))

  (use-package haskell-process
    :config
    (setq haskell-process-type 'cabal-repl)))

(use-package lsp-haskell
  :straight t
  :hook
  (haskell-mode-hook . lsp)
  (haskell-literate-mode-hook . lsp)
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  (add-to-list 'lsp-enabled-clients 'lsp-haskell))

(defconst haskell-mode-leader "C-c c")

(general-create-definer haskell-mode-def
  :prefix haskell-mode-leader)

(haskell-mode-def
 :keymaps 'haskell-mode-map
 "c" 'haskell-compile
 "l" 'haskell-process-load-or-reload
 "`" 'haskell-interactive-bring
 "b" 'haskell-process-cabal-build
 "p" 'haskell-process-cabal)

(haskell-mode-def
 :keymaps 'haskell-cabal-mode-map
 "c" 'haskell-compile)

(general-define-key
 :keymaps 'interactive-haskell-mode-map
 "M-." 'haskell-mode-goto-loc)

(provide 'lang-haskell)
;;; lang-haskell.el ends here
