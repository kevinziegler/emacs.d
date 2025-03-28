(use-package evil
  :demand t
  :general
  (kdz/leader-def
    "j"  `("Jump to <thing>"   . ,(make-sparse-keymap))
    "jj" '("Jump to line"      . evil-avy-goto-line)
    "jc" '("Jump to character" . evil-avy-goto-char)
    "jw" '("Jump to word"      . evil-avy-goto-word-0))

  (kdz/leader-buffer-def "N" '("New Buffer" . evil-buffer-new))

  (kdz/leader-window-def
    "d"  '("Delete Window"     . evil-window-delete)
    "j"  '("Down"              . evil-window-down)
    "k"  '("Up"                . evil-window-up)
    "n"  '("New Window"        . evil-window-new)
    "x"  '("Exchange Window"   . evil-window-exchange)
    "H"  '("Move window left"  . evil-window-move-far-left)
    "L"  '("Move window right" . evil-window-move-far-right)
    "J"  '("Move window down"  . evil-window-move-far-down)
    "K"  '("Move window up"    . evil-window-move-far-up))

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
  :after (evil smartparens)
  :hook (lisp-mode . evil-cleverparens-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (setq evil-collection-magit-want-horizontal-movement t))

(use-package evil-embrace
  :hook (org-mode . embrace-org-mode-hook)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :init
  (evil-escape-mode 1))

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-pulse t)
  (setq evil-goggles-blocking-duration 0.300)
  (setq evil-goggles-async-duration 0.900)
  (evil-goggles-mode))

(use-package evil-lion
  :config
  (evil-lion-mode))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :general (general-def :states '(normal visual) "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Set up text object mappings as keybindings (see project README)
(use-package evil-textobj-tree-sitter
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


(use-package evil-numbers
  :general
  (general-def
    :states 'normal
    "+" '(evil-numbers/inc-at-pt :which-key "Increment at point")
    "=" '(evil-numbers/inc-at-pt :which-key "Increment at point")
    "-" '(evil-numbers/dec-at-pt :which-key "Decrement at point")
    "_" '(evil-numbers/dec-at-pt :which-key "Decrement at point")))


(use-package evil-exchange
  :config
  (setq evil-exchange-key (kbd "zX"))
  (evil-exchange-install))

(use-package evil-quick-diff
  :ensure (evil-quick-diff :host github :repo "rgrinberg/evil-quick-diff")
  :config
  (setq evil-quick-diff-key (kbd "zx"))
  (evil-quick-diff-install))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500
        evil-owl-idle-delay 0.5)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(provide 'packages.d/evil)
