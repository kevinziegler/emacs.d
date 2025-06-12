(use-package lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-arrow "❱"
        lsp-session-file (kdz/user-directory ".local/lsp-session-v1"))
  :hook ((python-ts-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands (lsp lsp-deferred))

(use-package lsp-origami
  :after (lsp-mode origiami)
  :hook (lsp-after-open . #'lsp-origami-try-enable))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-alignment 'window
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-header t
        lsp-ui-doc-max-height 25
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :after lsp-mode
  :general
  (kdz/leader-code-lookup-def "l" '("Symbols List" . lsp-treemacs-symbols)))

(use-package consult-lsp :after (lsp-mode treemacs))
(use-package lsp-docker :after lsp-mode)
(use-package lsp-java :after lsp-mode)
(use-package lsp-pyright
  :after lsp-mode
  :init
  (setopt lsp-pyright-langserver-command "basedpyright"))

(use-package dap-mode
  :init
  (setq dap-breakpoints-file (kdz/user-directory ".local/dap-breakpoints")))

(provide 'packages.d/tools/lsp)
