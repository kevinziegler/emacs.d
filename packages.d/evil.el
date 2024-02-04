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
  (defun +evil-update-cursor-color-h ()
    "Update cursor colors to indicate evil state

The colors are based on face colors from the current theme; this
function is intended to run as a hook after changing Emacs' theme.

NOTE This function is lifted from Doom Emacs' evil module"
    (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
    (put 'cursor 'evil-normal-color (face-background 'cursor)))

  (defun +evil-default-cursor-fn ()
    "Get color to use for the cursor when in evil's `normal' state

NOTE This function is lifted from Doom Emacs' evil module"
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

  (defun +evil-emacs-cursor-fn ()
    "Get color to use for the cursor when in evil's `emacs' state

NOTE This function is lifted from Doom Emacs' evil module"
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))
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
(use-package evil-textobj-tree-sitter :straight t)

(use-package evil-anzu :straight t :after anzu)

;;(straight-use-package 'evil-numbers)

;; (use-package evil-easymotion
;;   :straight t
;;   :config
;;   (evilem-default-keybindings "SPC j"))

;; (straight-use-package 'evil-exchange)
;; (straight-use-package 'evil-indent-plus)
;; (straight-use-package 'evil-snipe)
;; (straight-use-package '(evil-textobj-anyblock
;;                         :type git
;;                         :host github
;;                         :repo "willghatch/evil-textobj-anyblock"
;;                         :branch "fix-inner-block"))
;; (straight-use-package 'evil-traces)
;; (straight-use-package 'exato)
;; (straight-use-package '(evil-quick-diff
;;                         :type git
;;                         :host github
;;                         :repo "rgrinberg/evil-quick-diff"))
