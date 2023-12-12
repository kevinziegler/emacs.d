(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

(use-package plantuml-mode
  :straight t
  :config
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.pu?ml\\'" . plantuml-mode)))

(use-package lua-mode
  :straight t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package toml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode)))

(use-package scad-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.scad\\'" . scad-mode)))

(use-package dotenv-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package php-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

(use-package kotlin-ts-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode)))

(use-package markdown-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
