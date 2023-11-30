(use-package evil
  :demand t
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'before
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t
	evil-move-beyond-eol t)
  :init 
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-visual-update-x-selection-p nil)
  (unless noninteractive (setq save-silently t)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :ensure t
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-escape
  :init (evil-escape-mode 1))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys t))

(use-package tabspaces
  :config
  (delete 'tab-bar-format-add-tab tab-bar-format)
  (tab-bar-select-tab-by-name "Home")
  (tab-bar-close-tab-by-name "*scratch*"))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful)

(use-package which-key
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-prefix-prefix "☰ ")
  (which-key-mode))

(use-package vertico
  :init
  (vertico-mode)
  (advice-add #'vertico--format-candidate
              :around #'kdz/vertico--format-candiate-marker-advice))

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook ((fundamental-mode) . indent-bars-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-hide-gitignored-files-mode 1)
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
         #'command-completion-default-include-p)
  (setq tab-always-indent 'complete))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode))

(use-package evil-cleverparens
  :after (evil smartparens)
  :config
  (add-hook 'lisp-mode-hook #'evil-cleverparens-mode))

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-pulse t)
  (setq evil-goggles-blocking-duration 0.300)
  (setq evil-goggles-async-duration 0.900)
  (evil-goggles-mode))

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

(use-package vertico-posframe
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8))))

(use-package marginalia
  :init (marginalia-mode))

(use-package svg-tag-mode
  :config
  (svg-tag-mode)
  (let ((tab-num-regexp "\\[tab-\\([0-9]+\\)\\]" ))
    (setq svg-tag-tags '(("\\[tab-\\([0-9]+\\)\\]"  . ((lambda (tag)
					      (save-match-data
						(string-match "\\[tab-\\([0-9]+\\)\\]")
						(svg-tag-make (match-string 1 tag))))))))))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 7
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 7
         :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package catppuccin-theme)

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

;; (use-package mood-line
;;   :config
;;   (setq mood-line-format mood-line-format-default))
;; (use-package moody
;;   :config
;;   ;; Other face changes:
;;   ;; - Remove box around mode line
;;   ;; - Set overline & underline on faces
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))
