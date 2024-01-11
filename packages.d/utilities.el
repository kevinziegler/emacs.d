;; UI-related elements, themes, etc.
(use-package catppuccin-theme :straight t)

(use-package dashboard
  :init
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (setq dashboard-items '((projects . 5) (bookmarks . 5))
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-projects-backend 'project-el
        dashboard-projects-switch-function
	#'tabspaces-open-or-create-project-and-workspace)
  (dashboard-setup-startup-hook))

(use-package dashboard-project-status :straight t)
(use-package dashboard-ls :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
	doom-modeline-persp-icon nil
	doom-modeline-persp-name nil
	doom-modeline-buffer-encoding nil))

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
  :after 'catppuccin-theme
  :config
  (solaire-global-mode +1))

;; TODO Configure for org-mode
(use-package svg-tag-mode
  :straight t
  :config
  (global-svg-tag-mode))

(use-package tabspaces
  :straight t
  :config
  (setq tabspaces-session-file (expand-file-name ".local/tabsession.el"
						 user-emacs-directory))
  (delete 'tab-bar-format-add-tab tab-bar-format)
  (add-to-list 'tabspaces-exclude-buffers dashboard-buffer-name)

  (advice-add 'tabspaces-open-or-create-project-and-workspace
              :after
              (lambda (&rest _) (tabspaces-reset-buffer-list)))

  ;; Set this variable to skip buffers that wouldn't show up in the
  ;; current tab per tabspaces's rules, to avoid buffers "leaking"
  ;; into the current space from other spaces.
  (setq switch-to-prev-buffer-skip
        (lambda (window buffer bury-or-kill)
          (not (tabspaces--local-buffer-p buffer))))

  (tab-bar-select-tab-by-name "Home")
  (tab-bar-close-tab-by-name "*scratch*"))

(use-package treemacs
  :straight t
  :defer t
  :config
  (treemacs-hide-gitignored-files-mode 1)
  (add-hook 'treemacs-mode-hook
            (lambda () (display-line-numbers-mode -1))))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-tab-bar
  :straight t
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

;; Minibuffer/completion utlities
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package consult
  :straight t
  :config
  (setq consult-narrow-key "<"))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)

  ;; https://github.com/emacs-evil/evil-collection/issues/766
  (advice-remove 'corfu--setup 'evil-normalize-keymaps)
  (advice-remove 'corfu--teardown 'evil-normalize-keymaps)

  (advice-add 'corfu--setup :after (lambda (&rest r) (evil-normalize-keymaps)))
  (advice-add 'corfu--teardown :after  (lambda (&rest r) (evil-normalize-keymaps))))

(use-package embark
  :straight t
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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (advice-add #'vertico--format-candidate
              :around #'kdz/vertico--format-candiate-marker-advice))

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-posframe
  :straight t
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8))))

;; Miscellaneous
(use-package helpful :straight t)

(use-package general :straight t)

(use-package which-key
  :straight t
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-sort-uppercase-first nil
	which-key-prefix-prefix "☰ ")
  (which-key-mode))

(use-package emacs
  :init
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (set-charset-priority 'unicode)

  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)

  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate #'command-completion-default-include-p
        tab-always-indent 'complete)

  ;; Use `y' and `n' prompts instead of `yes' and `no'
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)
	 (ielm-mode . electric-pair-mode)))

(use-package git-gutter-fringe
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package git-modes
  :straight t
  :config
  (add-to-list 'auto-mode-alist (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode))

;; TODO Set up keybindings
(use-package browse-at-remote
  :straight t)

(use-package modern-fringes
  :config
  (fringe-mode)
  (modern-fringes-mode)
  (modern-fringes-invert-arrows))

(use-package f :straight t)

(use-package origami
  :straight t
  :config
  (setq origami-fold-replacement " ... ")
  (global-origami-mode))

(use-package magit
  :straight t)

(use-package magit-delta
  :straight t
  :hook (magit-mode . magit-delta-mode))

(use-package git-timemachine
  :straight t)

(use-package f
  :straight t)

(use-package repl-toggle
  :straight t
  :config
  (setq rtog/fullscreen t
	rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
			       (python-mode . run-python)
			       (python-ts-mode . run-python))))

(use-package ibuffer-project
  :straight t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
	      (unless (eq ibuffer-sorting-mode 'project-file-relative)
		(ibuffer-do-sort-by-project-file-relative))))
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

(use-package list-environment
  :straight t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package undo-fu
  :straight t
  :config
  (setopt evil-undo-system 'undo-fu))

(use-package vundo :straight t)

(use-package perfect-margin :straight t)

(use-package imenu-list :straight t)

(use-package file-info
  :straight t
  :config
  (setq file-info-headline-underline-symbol ?━))

(use-package hydra
  :straight t
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `(:poshandler posframe-poshandler-frame-center
                      :internal-border-width 2
                      :internal-border-color "#61AFEF"
                      :left-fringe 16
                      :right-fringe 16)))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets :straight t :after 'yasnippet)

(use-package consult-yasnippet :straight t :after 'yasnippet)

(use-package jinx
  :straight t
  :config
  (global-jinx-mode))

(use-package gcmh
  :straight t
  :demand
  :config
  (gcmh-mode 1))

(use-package ws-butler
  :straight t
  :hook ((prog-mode . ws-butler-mode)))

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(use-package copy-as-format
  :straight t)

(use-package markdown-xwidget
  :after markdown-mode
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources")))

(use-package abridge-diff
  :after magit ;; optional, if you'd like to use with magit
  :init (abridge-diff-mode 1))

(use-package sideline-blame :straight t)

(use-package sideline-lsp
  :straight t)

(use-package sideline
  :straight t
  :init
  (setq sideline-backends-right '((sideline-blame . down)))
  :config
  (global-sideline-mode 1))

(use-package editorconfig
  :straight t
  :config
  (setopt editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (editorconfig-mode 1))

(use-package project
  :config
  (advice-add 'project-switch-project
              :before
              #'kdz/project-open-show-dashboard)
  (advice-add 'project-switch-project
              :after
              #'kdz/project-kill-dashboard))


;; TODO This needs tree-sitter to work
;; TODO This pulls from quelpa; how do I set that up with straight?
;; (use-package turbo-log
;;   :straight t
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))

(use-package anzu
  :straight t
  :config
  (global-anzu-mode +1))
