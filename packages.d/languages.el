(use-package dotenv-mode :straight t :mode (("\\.env\\..*\\'" . dotenv-mode)))

(use-package kotlin-ts-mode :straight t :mode (("\\.kt\\'" . kotlin-ts-mode)))

(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter (("lua" . lua-mode))
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t))

(use-package markdown-mode :straight t :mode (("\\.md\\'" . markdown-mode)))

(use-package php-mode :straight t :mode (("\\.php\\'" . php-mode)))

(use-package plantuml-mode
  :straight t
  :mode (("\\.plantuml\\'" . plantuml-mode)
	 ("\\.pu?ml\\'"    . plantuml-mode))
  :config
  (setq plantuml-default-exec-mode 'executable))

(use-package scad-mode :straight t :mode (("\\.scad\\'" . scad-mode)))

(use-package toml-mode :straight t :mode (("\\.toml\\'" . toml-mode)))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.hbs\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)  
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package yaml-mode :straight t :mode (("\\.ya?ml\\'" . yaml-mode)))

(use-package jq-mode :straight t :mode (("\\.jq\\'" . jq-mode)))

(use-package tree-sitter :straight t)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :config
  (require 'tree-sitter)
  (require 'tree-sitter-langs))

(use-package treesit-auto
  :straight t
  :after tree-sitter
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
