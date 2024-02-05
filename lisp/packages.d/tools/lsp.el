(use-package lsp-mode
  :straight t
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-session-file (kdz/user-directory ".local/lsp-session-v1"))
  :hook ((python-ts-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-origami
  :straight t
  :after '(lsp-mode origiami)
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :init
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-treemacs :straight t :after lsp-mode)
(use-package consult-lsp :straight t :after '(lsp-mode treemacs))
(use-package lsp-docker :straight t :after lsp-mode)
(use-package lsp-java :straight t :after lsp-mode)
(use-package lsp-pyright :straight t :after lsp-mode)

(use-package dap-mode
  :straight t
  :init
  (setq dap-breakpoints-file (kdz/user-directory ".local/dap-breakpoints")))
