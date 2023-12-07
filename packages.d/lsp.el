(use-package lsp-mode
  :straight t
  :init
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  :hook ((python-ts-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-origami
  :straight t
  :after '(lsp-mode origiami)
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package lsp-ui :straight t)
(use-package lsp-treemacs :straight t)
(use-package consult-lsp :straight t)
(use-package lsp-docker :straight t)
(use-package lsp-java :straight t)
(use-package lsp-pyright :straight t)
