(use-package evil
  :straight t
  :demand t
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'before
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        ;; evil-default-cursor '+evil-default-cursor-fn
        evil-default-cursor      'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-emacs-state-cursor  'box
        ;; evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
        evil-split-window-below t
        evil-vsplit-window-right t
	evil-move-beyond-eol t)
  :init
  (setq evil-want-keybinding nil)
  :config

  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-visual-update-x-selection-p nil)
  (unless noninteractive (setq save-silently t)))

(use-package evil-args
  :straight t
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-cleverparens
  :straight t
  :after (evil smartparens)
  :config
  (add-hook 'lisp-mode-hook #'evil-cleverparens-mode))

(use-package evil-collection
  :straight t
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-embrace
  :straight t
  :ensure t
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :straight t
  :init
  (evil-escape-mode 1))

(use-package evil-goggles
  :straight t
  :after evil
  :config
  (setq evil-goggles-pulse t)
  (setq evil-goggles-blocking-duration 0.300)
  (setq evil-goggles-async-duration 0.900)
  (evil-goggles-mode))

(use-package evil-lion
  :straight t
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-matchit
  :straight t
  :init
  (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter :straight t)

(use-package evil-surround
  :straight t
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Set up text object mappings as keybindings (see project README)
(use-package evil-textobj-tree-sitter
  :straight t
  :config
  (define-key evil-outer-text-objects-map
              "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map
              "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map
              "a"
              (evil-textobj-tree-sitter-get-textobj
                ("conditional.outer" "loop.outer"))))

(use-package evil-anzu :straight t :after anzu)

(use-package evil-numbers :straight t)

(use-package exato :straight t)

(use-package evil-exchange
  :straight t
  :config
  (setq evil-exchange-key (kbd "zX"))
  (evil-exchange-install))

(use-package evil-quick-diff
  :straight '(:type git :host github :repo "rgrinberg/evil-quick-diff")
  :config
  (setq evil-quick-diff-key (kbd "zx"))
  (evil-quick-diff-install))

(use-package evil-visualstar
  :straight t
  :config
  (global-evil-visualstar-mode))

(use-package evil-owl
  :straight t
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(provide 'packages.d/evil)
