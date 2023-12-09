;; UI-related elements, themes, etc.
(use-package catppuccin-theme
  :straight t)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (setq dashboard-items '((projects . 5) (bookmarks . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-projects-switch-function #'tabspaces-open-or-create-project-and-workspace)
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
	doom-modeline-persp-icon nil
	doom-modeline-persp-name nil
	doom-modeline-buffer-encoding nil))

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook ((fundamental-mode) . indent-bars-mode))

(use-package spacious-padding
  :straight t
  :config
  (setq spacious-padding-widths
	'(:internal-border-width 7
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 7
          :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

(use-package svg-tag-mode
  :straight t
  :config
  (svg-tag-mode)
  (let ((tab-num-regexp "\\[tab-\\([0-9]+\\)\\]" ))
    (setq svg-tag-tags '(("\\[tab-\\([0-9]+\\)\\]"  . ((lambda (tag)
					      (save-match-data
						(string-match "\\[tab-\\([0-9]+\\)\\]")
						(svg-tag-make (match-string 1 tag))))))))))

(use-package tabspaces
  :straight t
  :config
  (setq tabspaces-session-file (expand-file-name ".local/tabsession.el"
						 user-emacs-directory))
  (delete 'tab-bar-format-add-tab tab-bar-format)
  (tab-bar-select-tab-by-name "Home")
  (tab-bar-close-tab-by-name "*scratch*"))

(use-package treemacs
  :straight t
  :ensure t
  :defer t
  :config
  (treemacs-hide-gitignored-files-mode 1)
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :straight t
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil
  :straight t
  :after (treemacs evil)
  :ensure t)

;; Minibuffer/completion utlities
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package consult
  :straight t)

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package embark
  :straight t
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :init (marginalia-mode))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package orderless
  :straight t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (advice-add #'vertico--format-candidate
              :around #'kdz/vertico--format-candiate-marker-advice))

(use-package vertico-posframe
  :straight t
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8))))

;; Miscellaneous
(use-package helpful
  :straight t)

(use-package general
  :straight t)

(use-package which-key
  :straight t
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-prefix-prefix "☰ ")
  (which-key-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
         #'command-completion-default-include-p)
  (setq tab-always-indent 'complete))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1))

(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; TODO Set up keybindings
(use-package browse-at-remote
  :straight t)

;; (use-package modern-fringes
;;   :config
;;   (fringe-mode)
;;   (modern-fringes-mode)
;;   (modern-fringes-invert-arrows))

(use-package f)

(use-package origami
  :config
  (global-origami-mode))

(use-package magit
  :straight t)

(use-package git-timemachine
  :straight t)

(use-package f
  :straight t)
