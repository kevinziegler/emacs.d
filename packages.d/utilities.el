;;; Core Emacs Configuration
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

  ;; Emacs 28: Hide commands in M-x which do not apply to the current
  ;;           mode.  Corfu commands are hidden, since they are not
  ;;           supposed to be used via M-x.
  (setq read-extended-command-predicate #'command-completion-default-include-p
        tab-always-indent 'complete)

  ;; Use `y' and `n' prompts instead of `yes' and `no'
  (defalias 'yes-or-no-p 'y-or-n-p))

;;;; Instance-level behaviors
(use-package list-environment :straight t)
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;; Libraries
(use-package f :straight t)

;;; UI improvements and extras
;;;; Foundational UI Packages
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

(use-package sideline
  :straight t
  :init
  (setq sideline-backends-right '((sideline-blame . down)))
  (setq sideline-backends-right-skip-current-line nil)
  :config
  (global-sideline-mode 1))

(use-package nerd-icons :straight t)

;;;; Cosmetic extras, themes, etc.
(use-package catppuccin-theme :straight t)
(use-package creamsody-theme :straight t)

(use-package spacious-padding
  :straight t
  :config
  (setq spacious-padding-widths (list :internal-border-width 7
                                      :header-line-width 4
                                      :mode-line-width 6
                                      :tab-width 4
                                      :right-divider-width 7
                                      :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package solaire-mode
  :straight t
  :after catppuccin-theme
  :config
  (solaire-global-mode +1))

(use-package svg-tag-mode :straight t :config (global-svg-tag-mode))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

;;;; Dashboard-related packages
(use-package dashboard
  :straight t
  :after nerd-icons
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

(use-package dashboard-ls
  :after dashboard
  :straight t
  :config
  (add-to-list 'dashboard-heading-icons
               '(ls-directories . "nf-oct-file_directory"))
  (add-to-list 'dashboard-heading-icons
               '(ls-files . "nf-oct-file")))

(use-package dashboard-project-status :after dashboard :straight t)

;;;; Use Doom's Modeline
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
	doom-modeline-persp-icon nil
	doom-modeline-persp-name nil
	doom-modeline-buffer-encoding nil))

;;;; Workspace/project management
(use-package project
  :config
  (defun kdz/project-dashboard-buffer (project-dir)
    "Generate a project-specific name for the dashboard buffer"
    (format "*dashboard: %s*" project-dir))

  (defun kdz/project-open-show-dashboard (project-dir)
    "Open a new dashboard buffer for the supplied PROJECT-DIR"
    (when (and project-dir (member (list project-dir) project--list))
      (let ((dashboard-buffer-name (kdz/project-dashboard-buffer project-dir))
            (dashboard-item-generators dashboard-item-generators)
            (dashboard-items '((ls-directories . 5)
                               (ls-files . 5))))
        (push `(project-status . ,(dashboard-project-status project-dir))
              dashboard-item-generators)
        (dashboard-open))))

  (defun kdz/project-kill-dashboard (project-dir)
    "Kill the dashboard buffer for the supplied PROJECT-DIR"
    (let ((dashboard (kdz/project-dashboard-buffer project-dir)))
      (when (get-buffer dashboard)
        (kill-buffer dashboard))))

  (advice-add 'project-switch-project
              :before
              #'kdz/project-open-show-dashboard)
  (advice-add 'project-switch-project
              :after
              #'kdz/project-kill-dashboard))

(use-package tab-bar
  :straight nil
  :config
  (kdz/init "lib/tab-bar.el")

  ;; TODO Figure out why this isn't affecting the behavior for
  ;;      switching tabs correctly.  Once that's done, I *should*
  ;;      be able to use the partitioned tabs correctly.
  ;; (advice-add tab-bar-tabs-function
  ;;             :filter-return
  ;;             #'kdz/tab-bar-tabs-sort-pinned-tabs-last)
  (defun kdz/tab-bar-update-faces (&rest _)
    "Customize tab-bar faces against current theme

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (set-face-attribute 'tab-bar nil
		        :inherit 'mode-line
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)
		        :box `(:line-width 7 :color ,(face-background 'default)))
    (set-face-attribute 'tab-bar-tab nil
		        :inherit 'mode-line
		        :height 1.0
		        :underline `(:color ,(face-background 'match) :position -7)
		        :foreground (face-foreground 'mode-line))
    (set-face-attribute 'tab-bar-tab-inactive nil
		        :inherit 'mode-line
		        :height 1.0
		        :foreground (face-foreground 'mode-line-inactive)))

  (defun kdz/tab-switch-index-or-select (&optional index)
    "Change tabs, optionally by index using a prefix argument"
    (interactive "P")
    (if (eq index nil)
        (call-interactively 'tab-switch)
      (tab-bar-select-tab index)))

  (defun kdz/create-named-tab (tab-name)
    "Create a named tab with a new scratch buffer"
    (interactive "sName for new tab: ")
    (tab-bar-new-tab)
    (switch-to-buffer (generate-new-buffer (format "*scratch: %s*"
                                                   tab-name)))
    (tab-bar-rename-tab tab-name))

  (setq tab-bar-new-tab-to 'rightmost
        tab-bar-format '(tab-bar-separator
                         kdz/tab-bar-format-project-icon
                         tab-bar-separator
                         kdz/tab-bar-format-unpinned-tabs
                         tab-bar-format-align-right
                         tab-bar-separator
                         kdz/tab-bar-format-pinned-tabs
                         tab-bar-separator
                         kdz/tab-bar-format-pin-icon)))

(use-package tabspaces
  :straight t
  :config
  (setq tabspaces-session-file (expand-file-name ".local/tabsession.el"
						 user-emacs-directory))
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

(use-package ibuffer-project
  :straight t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups))
	      (unless (eq ibuffer-sorting-mode 'project-file-relative)
		(ibuffer-do-sort-by-project-file-relative))))
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups)))))

;;;; Treemacs as a sidebar explorer
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
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil :straight t :after (treemacs evil))

;;;; Minibuffer Completion (Consult, Vertico, etc)
(use-package consult
  :straight t
  :config
  (setq consult-narrow-key "<"))

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
  :hook ((embark-collect-mode . consult-preview-at-point-mode)))

(use-package marginalia :straight t :init (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :init
  (vertico-mode)

  (defun kdz/vertico--format-candiate-marker-advice
      (orig cand prefix suffix index start)
    (setq cand (funcall orig cand prefix suffix index start))
    (concat (if (= vertico--index index)
                (propertize "» " 'face 'vertico-current)
              "  ")
            cand))

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
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package vertico-posframe
  :straight t
  :after '(posframe vertico)
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler #'kdz/posframe-offset-top)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8))))

;;;; Completion-at-point (Cape/Corfu)
(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode)

  ;; https://github.com/emacs-evil/evil-collection/issues/766
  (advice-remove 'corfu--setup 'evil-normalize-keymaps)
  (advice-remove 'corfu--teardown 'evil-normalize-keymaps)

  (advice-add 'corfu--setup
              :after (lambda (&rest r) (evil-normalize-keymaps)))
  (advice-add 'corfu--teardown
              :after (lambda (&rest r) (evil-normalize-keymaps))))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; Help/Discoverability
(use-package helpful :straight t)
(use-package general :straight t)

(use-package posframe
  :straight t
  :config
  (defvar kdz--posframe-offset-top-percent 10)
  (defvar kdz--posframe-offset-bottom-percent 10)

  (defun kdz/posframe-center-width (info)
    (round
     (* 0.5 (- (plist-get info :parent-frame-width)
               (plist-get info :posframe-width)))))

  (defun kdz/posframe-offset-top (info)
    (let ((offset-percent (/ kdz--posframe-offset-top-percent 100.0))
          (frame-height (plist-get info :parent-frame-height)))
      (cons (kdz/posframe-center-width info)
            (round (* offset-percent frame-height)))))

  (defun kdz/posframe-offset-bottom (info)
    (let* ((parent-frame-height (plist-get info :parent-frame-height))
           (posframe-height (plist-get info :posframe-height))
           (offset-percent (/ kdz--posframe-offset-bottom-percent 100.0)))
      (cons (kdz/posframe-center-width info)
            (round (- parent-frame-height
                      posframe-height
                      (* offset-percent parent-frame-height)))))))

(use-package which-key
  :straight t
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-sort-uppercase-first nil
	which-key-prefix-prefix "☰ ")
  (which-key-mode))

(use-package which-key-posframe
  :straight t
  :config
  ;; Stand-in until the following issue is merged:
  ;; https://github.com/yanghaoxie/which-key-posframe/pull/21
  ;;
  ;; Note that my version *also* tweaks the width parameter
  (defun kdz/fixup--which-key-posframe--show-buffer (act-popup-dim)
    "Override which-key-posframe parameters to ensure content is visible"
    (when (posframe-workable-p)
      (save-window-excursion
        (posframe-show
         which-key--buffer
         :font which-key-posframe-font
         :position (point)
         :poshandler which-key-posframe-poshandler
         :background-color (face-attribute 'which-key-posframe :background nil t)
         :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
         :height (ceiling (* 1.25 (car act-popup-dim)))
         :width (ceiling (* 1.1 (cdr act-popup-dim)))
         :internal-border-width which-key-posframe-border-width
         :internal-border-color (face-attribute 'which-key-posframe-border
                                                :background nil
                                                t)
         :override-parameters which-key-posframe-parameters))))

  (advice-add #'which-key-posframe--show-buffer
              :override
              #'kdz/fixup--which-key-posframe--show-buffer)

  (setq which-key-posframe-poshandler 'kdz/posframe-offset-bottom)
  (which-key-posframe-mode 1))


;;;; In-Buffer UI Enhancments - Editing behaviors, formating, etc
(use-package browse-at-remote :straight t) ;; TODO Set up keybindings
(use-package perfect-margin :straight t)
(use-package imenu-list :straight t)
(use-package yasnippet :straight t :config (yas-global-mode))
(use-package yasnippet-snippets :straight t :after 'yasnippet)
(use-package consult-yasnippet :straight t :after 'yasnippet)
(use-package copy-as-format :straight t)
(use-package separedit :straight t)
(use-package expand-region :straight t)
(use-package jinx :straight t :config (global-jinx-mode))
(use-package apheleia :straight t :config (apheleia-global-mode +1))
(use-package vundo :straight t)
(use-package eval-sexp-fu :straight t)
(use-package anzu :straight t :config (global-anzu-mode +1))
(use-package sideline-blame :straight t)
(use-package sideline-lsp :straight t)

(use-package hl-todo
  :straight t
  :config
  (defun kdz/set-hl-todo-faces (&rest _)
    "Set face colors for hl-todo keywords

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (setq hl-todo-keyword-faces
          `(("TODO"   . ,(face-foreground 'hl-todo))
            ("FIXME"  . ,(face-foreground 'ansi-color-red))
            ("DEBUG"  . ,(face-foreground 'ansi-color-cyan))
            ("NOTE"   . ,(face-foreground 'ansi-color-blue))
            ("STUB"   . ,(face-foreground 'ansi-color-green)))))
  (global-hl-todo-mode))

(use-package hide-mode-line
  :straight t
  :hook ((reb-mode . hide-mode-line-mode)))

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)
	 (ielm-mode . electric-pair-mode)))

(use-package git-gutter-fringe
  :straight t
  :config
  (global-git-gutter-mode 1))

(use-package modern-fringes
  :config
  (fringe-mode)
  (modern-fringes-mode)
  (modern-fringes-invert-arrows))

(use-package origami
  :straight t
  :config
  (setq origami-fold-replacement " ... ")
  (global-origami-mode))

(use-package repl-toggle
  :straight t
  :config
  (setq rtog/fullscreen nil
        rtog/fallback-repl-fun #'ielm
	rtog/mode-repl-alist '((emacs-lisp-mode . ielm)
			       (python-mode . run-python)
			       (python-ts-mode . run-python))))

(use-package ws-butler
  :straight t
  :hook ((prog-mode . ws-butler-mode)))

(use-package editorconfig
  :straight t
  :config
  (setopt editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (editorconfig-mode 1))

(use-package undo-fu
  :straight t
  :config
  (setopt evil-undo-system 'undo-fu))

(use-package file-info
  :straight t
  :config
  (setq file-info-headline-underline-symbol ?━))

(use-package markdown-xwidget
  :after markdown-mode
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources")))

(use-package display-line-numbers
  :straight nil
  :config
  (defun kdz/toggle-line-numbers ()
    "Cycle between relative/absolute line numbers"
    (interactive)
    (if display-line-numbers
        (setq display-line-numbers
	      (if (eq display-line-numbers 'relative) t 'relative))
      (message "Line numbers are currently disabled!"))))

(use-package display-line-numbers
  :straight nil
  :config
  (defun kdz/toggle-line-numbers ()
    "Cycle between relative/absolute line numbers"
    (interactive)
    (if display-line-numbers
        (setq display-line-numbers
	      (if (eq display-line-numbers 'relative) t 'relative))
      (message "Line numbers are currently disabled!"))))

;; TODO This needs tree-sitter to work
;; TODO This pulls from quelpa; how do I set that up with straight?
;; (use-package turbo-log
;;   :straight t
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))

;;; Git/Version Control Tooling
(use-package magit :straight t)
(use-package git-timemachine :straight t)
(use-package abridge-diff :after magit :init (abridge-diff-mode 1))

(use-package magit-delta
  :straight t
  :hook ((magit-mode . magit-delta-mode)))

(use-package git-modes
  :straight t
  :mode (("/.dockerignore\\'" . gitignore-mode )))

;; TODO This needs tree-sitter to work
;; TODO This pulls from quelpa; how do I set that up with straight?
;; (use-package turbo-log
;;   :straight t
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))
