;;;; In-Buffer UI Enhancments - Editing behaviors, formating, etc
(use-package browse-at-remote :straight t) ;; TODO Set up keybindings
(use-package perfect-margin :straight t)
(use-package imenu-list :straight t)
(use-package yasnippet :straight t :config (yas-global-mode))
(use-package yasnippet-snippets :straight t :after yasnippet)
(use-package apheleia :straight t :config (apheleia-global-mode +1))
(use-package anzu :straight t :config (global-anzu-mode +1))
(use-package flycheck :straight t)

(use-package vundo
  :straight t
  :general
  (kdz/leader-buffer-def "h" '("Undo History" . vundo)))

(use-package consult-yasnippet
  :straight t
  :after yasnippet
  :general
  (kdz/leader-insert-def "s" '("Snippet" . consult-yasnippet)))

(use-package eval-sexp-fu
  :straight t
  :general
  (kdz/leader-code-eval-def
    "s" '("Evaluate s-exp" . eval-sexp-fu-eval-sexp-inner-list)))

(use-package expand-region
  :straight t
  :general
  (kdz/leader-edit-def "e" '("Expand Region" . er/expand-region)))

(use-package separedit
  :straight t
  :general
  (kdz/leader-edit-def "b" '("Edit block in separate buffer" . separedit)))

(use-package sideline
  :straight t
  :general
  (kdz/leader-toggle-def
    "s" '("Show/hide Sideline" . sideline-mode))
  (kdz/leader-toggle-global-def
    "s" '("Show/hide Sideline" . global-sideline-mode)))

(use-package copy-as-format
  :straight t
  :general
  (kdz/leader-edit-def
    "y"   (cons "Copy as <format>" (make-sparse-keymap))
    "yj" '("Copy as JIRA" . copy-as-format-jira)
    "yh" '("Copy as HTML" . copy-as-format-html)
    "ys" '("Copy as Slack" . copy-as-format-slack)
    "ym" '("Copy as Markdown (Plain)" . copy-as-format-markdown)
    "yg" '("Copy as Markdown (Github)" . copy-as-format-github)
    "yG" '("Copy as Markdown (Gitlab)" . copy-as-format-gitlab)))

(use-package jinx
  :straight t
  :general
  (general-def
    :states '(normal)
    :keymaps 'override
    :prefix "z"
    "=" '(jinx-correct :which-key "Correct Spelling"))
  :config
  (global-jinx-mode))

(use-package sideline-blame
  :straight t
  :after 'sideline
  :config (add-to-list 'sideline-backends-right sideline-blame))

(use-package sideline-flycheck
  :straight t
  :after '(sideline flycheck)
  :config (add-to-list 'sideline-backends-right sideline-flycheck))

(use-package sideline-lsp
  :straight t
  :after '(sideline lsp)
  :config (add-to-list 'sideline-backends-right sideline-lsp))

(use-package hl-todo
  :straight t
  :hook (kdz-load-theme . kdz/set-hl-todo-faces)
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
  :general
  (kdz/leader-code-def "r" '("Toggle REPL" . rtog/toggle-repl))
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
  :general
  (kdz/leader-file-def "i" '("Show Info" . file-info-show))
  :config
  (setq file-info-headline-underline-symbol ?━))

(use-package markdown-xwidget
  :after markdown-mode
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :config
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default") )

(use-package display-fill-column-indicator
  :init
  (global-display-fill-column-indicator-mode)
  :general
  (kdz/leader-toggle-def "c"
    '("Show/hide Sideline" . display-fill-column-indicator-mode))

  (kdz/leader-toggle-global-def "c"
    '("Show/hide fill column" . global-display-fill-column-indicator-mode))

  :config
  (dolist (mode '(dired-mode
                  dirvish-directory-view-mode
                  helpful-mode
                  markdown-mode
                  org-mode
                  special-mode))
    (add-to-list 'global-display-fill-column-indicator-modes `(not ,mode))))

(use-package display-line-numbers
  :init
  (global-display-line-numbers-mode)
  :general
  (kdz/leader-toggle-def
    "l" '("Show/hide line numbers"         . display-line-numbers-mode)
    "r" '("Relative/absolute line numbers" . kdz/toggle-line-numbers))
  (kdz/leader-toggle-global-def
    "l" '("Show/hide line numbers" . global-display-line-numbers-mode))

  :config
  (defun kdz/toggle-line-numbers ()
    "Cycle between relative/absolute line numbers"
    (interactive)
    (if display-line-numbers
        (setq display-line-numbers
	      (if (eq display-line-numbers 'relative) t 'relative))
      (message "Line numbers are currently disabled!")))

  (dolist (mode '(dashboard-mode-hook
                  dired-mode
                  dirvish-directory-view-mode
                  org-mode-hook
                  term-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode -1)))))

;; TODO Need to adjust face colors to contrast properly
(use-package highlight-indent-guides
  :straight t
  :general
  (kdz/leader-toggle-def
    "i" '("Show/hide indent guides" . highlight-indent-guides-mode)))

(use-package scroll-on-jump
  :straight t
  :after evil
  :config
  (setq scroll-on-jump-curve 'smooth-in)
  (setq scroll-on-jump-duration 0.6)
  (setq scroll-on-jump-curve-power 3.5)

  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  (scroll-on-jump-advice-add evil-goto-mark)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-goto-first-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))

;; TODO This needs tree-sitter to work
;; TODO This pulls from quelpa; how do I set that up with straight?
;; (use-package turbo-log
;;   :straight t
;;   :config
;;   (setq turbo-log-msg-format-template "\"KDZ-LOG: %s\""))

(use-package lorem-ipsum :straight t)
(use-package uuidgen :straight t)

(use-package fancy-urls-menu :straight t)
(provide 'packages.d/ui/buffer)
