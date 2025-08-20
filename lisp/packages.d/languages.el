(use-package dotenv-mode     :mode (("\\.env\\..*\\'" . dotenv-mode)))
(use-package scad-mode       :mode (("\\.scad\\'"     . scad-mode)))
(use-package restclient      :mode (("\\.http\\'"     . restclient-mode)))
(use-package awk-ts-mode     :mode (("\\.awk\\'"      . awk-ts-mode)))
(use-package scala-ts-mode   :mode (("\\.scala\\'"    . scala-ts-mode)))
(use-package jq-ts-mode      :mode (("\\.jq\\'"       . jq-ts-mode)))
(use-package kotlin-ts-mode  :mode (("\\.kt\\'"       . kotlin-ts-mode)))
(use-package groovy-mode)
(use-package hcl-mode)
(use-package terraform-mode)

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter (("lua" . lua-mode))
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t))

(use-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode))
  :config
  (setq markdown-enable-wiki-links t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t
        markdown-gfm-use-electric-backquote t
        markdown-italic-underscore t))

(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode)
	 ("\\.pu?ml\\'"    . plantuml-mode))
  :config
  (setq plantuml-default-exec-mode 'executable))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.hbs\\'"   . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package inputrc-mode)

(use-package treesit
  :ensure nil
  :mode (("Dockerfile" . dockerfile-ts-mode)
         ("\\.[tj]sx?\\'"  . tsx-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.css\\'"   . css-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.java\\'" . java-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.php\\'" . php-ts-mode)
         ("\\.py[iw]?\\'" . python-ts-mode)
         ("\\.toml\\'" . toml-ts-mode)
         ("\\.ya?ml\\'" . yaml-ts-mode)
         ("\\go.mode\\'" . go-mod-ts-mode)))

(use-package treesit-auto
  :config (global-treesit-auto-mode))

(use-package ielm
  :ensure nil
  :after nerd-icons
  :hook (elpaca-after-init . kdz/ielm-fancy-prompt)
  :config
  (defun kdz/ielm-fancy-prompt ()
    (let ((chevron (kdz/propertize-nerd-icon "nf-md-chevron_right_box")))
      (when (not (s-suffix? "\n" ielm-header ))
        (setq ielm-header (concat ielm-header "\n")))
      (setq ielm-prompt (concat "(elisp) " chevron " "))))
  (setq ielm-history-file-name (kdz/user-directory ".local" "ielm-history.eld")))

(use-package pyenv-mode
  :config
  (defun kdz/run-pyenv-python (force-unset)
    (interactive "P")
    (when (or force-unset (not (getenv "PYENV_VERSION")) )
      (call-interactively 'pyenv-mode-set))
    (call-interactively 'run-python)))

(use-package graphql-mode :mode ("\\.graphql\\'" "\\.gql\\'"))

(use-package swift-mode)
(use-package just-mode)

(use-package grpclient
  :if (executable-find "grpcurl")
  :ensure (grpclient :host github :repo "Prikaz98/grpclient.el")
  :init
  (add-to-list 'auto-mode-alist '("\\.grpc\\'" . grpclient-mode)))

(provide 'packages.d/languages)
