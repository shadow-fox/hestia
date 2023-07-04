;;; lsp-setup.el --- LSP Server Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package editorconfig)

(use-package emacs
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  ;;(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  )

(use-package emacs
  :init
  ;;(setq treesit-extra-load-path (concat hestia-misc-dir "/treesit"))
  :config
  (setq treesit-language-source-alist
        '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (c          "https://github.com/tree-sitter/tree-sitter-c/" "master" "src")
          (clojure    "https://github.com/sogaiu/tree-sitter-clojure" "master" "src")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp/" "master" "src")
          (cmake      "https://github.com/uyha/tree-sitter-cmake")
          (css        "https://github.com/tree-sitter/tree-sitter-css")
          (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
          (elixir     "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
          (erlang     "https://github.com/WhatsApp/tree-sitter-erlang" "main" "src")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (haskell    "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (java       "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (julia      "https://github.com/tree-sitter/tree-sitter-julia" "master" "src")
          (lua        "https://github.com/MunifTanjim/tree-sitter-lua" "main" "src")
          (make       "https://github.com/alemuller/tree-sitter-make")
          (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
          (meson      "https://github.com/Decodetalkers/tree-sitter-meson" "master" "src")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (ruby       "https://github.com/tree-sitter/tree-sitter-ruby" "master" "src")
          (rust       "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
          (toml       "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
 ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 ;; (mapc (lambda (lang) (funcall #'treesit-install-language-grammar (car lang) treesit-extra-load-path)) treesit-language-source-alist)
  )

(treesit-available-p)
(treesit-language-available-p 'rust)

(provide 'lsp-setup)

;;; lsp-setup.el ends here
