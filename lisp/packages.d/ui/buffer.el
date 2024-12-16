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
(use-package flycheck :straight t)
(use-package sideline :straight t)

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
             :files (:defaults "resources"))
  :config
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default") )

(use-package display-fill-column-indicator
  :init
  (global-display-fill-column-indicator-mode)
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
(use-package highlight-indent-guides :straight t)

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
