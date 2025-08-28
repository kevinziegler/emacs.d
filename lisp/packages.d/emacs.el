(use-package emacs
  :ensure nil
  :general
  (kdz/leader-code-eval-def
    "b" '("Evaluate Buffer"      . eval-buffer)
    "d" '("Evaluate Function"    . eval-defun)
    "r" '("Evaluate Region"      . eval-region))
  (kdz/leader-file-def
    "f" '("Find File"            . find-file))
  (kdz/leader-insert-def
    "u" '("Unicode Character"    . insert-char))
  (kdz/leader-frame-def
    "h" '("Move frame to left"   . kdz/place-frame-in-display-left)
    "w" '("Move frame to center" . kdz/place-frame-in-display-center)
    "l" '("Move frame to right"  . kdz/place-frame-in-display-right))

  :init
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (set-charset-priority 'unicode)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current
  ;;           mode.  Corfu commands are hidden, since they are not
  ;;           supposed to be used via M-x.
  (setq read-extended-command-predicate #'command-completion-default-include-p
        tab-always-indent 'complete)

  (setopt user-full-name "Kevin Ziegler")

  (let ((auto-save-dir (kdz/user-directory ".local" "auto-save"))
        (backup-dir (kdz/user-directory ".local" "backups")))

    (make-directory auto-save-dir t)
    (setf auto-save-list-file-prefix auto-save-dir
          auto-save-file-name-transforms
          `((".*" ,auto-save-dir t)))

    (make-directory backup-dir t)
    (setf backup-directory-alist
          `((".*" . ,backup-dir))))

  ;; files.el
  (setopt backup-by-copying t
          cursor-in-non-selected-windows nil
          delete-old-versions t
          kept-new-versions 5
          kept-old-versions 3
          version-control t)

  ;; C-sources
  (setopt coding-system-for-read 'utf-8
          coding-system-for-write 'utf-8
          default-process-coding-system '(utf-8-unix . utf-8-unix)
          delete-by-moving-to-trash t
          echo-keystrokes 0.25
          fill-column 80
          frame-inhibit-implied-resize t
          indicate-buffer-boundaries 'left
          inhibit-compacting-font-caches t
          locale-coding-system 'utf-8
          ring-bell-function 'ignore
          ;; show-trailing-whitespace t
          use-short-answers t
          x-stretch-cursor t
          x-underline-at-descent-line nil)

  ;; startup.el
  (setopt fancy-splash-image (expand-file-name "logo.png" user-emacs-directory))


  (setq-default history-length 1000
                prescient-history-length 1000)

  (defvar kdz-frame-side-offset 0.07
    "Offset to use when placing the frame on left side of the display")

  (defun kdz/place-frame-in-display-left ()
    "Move the current frame to the left side of the display"
    (interactive)
    (let ((position-x (round (* (display-pixel-width) kdz-frame-side-offset)))
          (position-y (/ (- (display-pixel-height) (frame-pixel-height)) 2)))
      (set-frame-position nil position-x position-y)))

  (defun kdz/place-frame-in-display-right ()
    "Move the current frame to the right side of the display"
    (interactive)
    (let* ((offset-x (round (* (display-pixel-width) kdz-frame-side-offset)))
           (position-x (- (display-pixel-width) (frame-pixel-width) offset-x))
           (position-y (/ (- (display-pixel-height) (frame-pixel-height)) 2)))
      (set-frame-position nil position-x position-y)))

  (defun kdz/place-frame-in-display-center ()
    "Move the current frame to the center of the display"
    (interactive)
    (let ((center-x (/ (- (display-pixel-width) (frame-pixel-width)) 2))
          (center-y (/ (- (display-pixel-height) (frame-pixel-height)) 2)))
      (set-frame-position nil center-x center-y))))

(use-package align
  :ensure nil
  :general
  (kdz/leader-edit-def "a" '("Align by Regexp" . align-regexp)))

(use-package apropos
  :ensure nil
  :config
  (setopt appropos-do-all t))

(use-package autorevert
  :ensure nil
  :config
  (setopt auto-revert-avoid-polling t
          auto-revert-check-vc-info t
          auto-revert-interval 5)
  (global-auto-revert-mode 1))

(use-package bookmark
  :ensure nil
  :config
  (setopt bookmark-file (kdz/user-directory ".local" "bookmarks")))

(use-package comint
  :ensure nil
  :config
  (setopt comint-prompt-read-only t
          comint-scroll-to-bottom-on-input t))

(use-package cus-edit
  :ensure nil
  :init
  (setopt custom-file (kdz/user-directory ".local" "custom.el")))

(use-package custom
  :ensure nil
  :init
  (defvar kdz-load-theme-hook nil
    "Hook to run actions after calling `load-theme'")
  :config
  (defmacro kdz/customize-with-palette (theme palette &rest face-specs)
    "Set custom FACE-SPECS for THEME with access to colors from PALETTE.

PALETTE is a symbol referencing either a function or alist mapping, and is
used to create a scoped function, (color NAME) that can be used to access colors
defined in that palette from within FACE-SPECS."
    `(cl-flet ((color (name)
                 (when (and (boundp ,palette))
                   (cond ((functionp ,palette) (funcall ,palette name))
                         ((and (listp (symbol-value ,palette))
                               (cl-every #'consp (symbol-value ,palette)))
                          (car (alist-get name (symbol-value ,palette))))
                         (t (error "Expected function or alist, but received neither."))))))
       (when (custom-theme-enabled-p ,theme)
         (let ((custom--inhibit-theme-enable nil))
           (custom-theme-set-faces ,theme ,@face-specs)))))

  (advice-add 'load-theme
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook))))

(use-package ediff
  :ensure nil
  :hook ((ediff-before-setup . kdz/store-pre-ediff-winconfig)
         (ediff-quit-hook    . kdz/restore-pre-ediff-winconfig))
  :config
  (setopt ediff-keep-variants nil
          ediff-split-window-function #'split-window-horizontally
          ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar kdz-ediff-last-windows nil)
  (defun kdz/store-pre-ediff-winconfig ()
    (setq kdz-ediff-last-windows (current-window-configuration)))

  (defun kdz/restore-pre-ediff-winconfig ()
    (set-window-configuration kdz-ediff-last-windows)))

(use-package eldoc
  :ensure nil
  :config
  :after evil
  ;; allow eldoc to trigger directly after changing modes
  (eldoc-add-command #'evil-normal-state
                     #'evil-insert
                     #'evil-change
                     #'evil-delete
                     #'evil-replace))

(use-package eshell
  :ensure nil
  :init
  (setq eshell-directory-name (kdz/user-directory ".local" "eshell")))

(use-package frame
  :ensure nil
  :general
  (kdz/leader-open-def "f" '("New Frame" . make-frame))
  (kdz/leader-toggle-def "f" '("Fullscreen" . toggle-frame-fullscreen))
  :config
  (let ((default-font-string  "Berkeley Mono 12"))
    (when (find-font (font-spec :name default-font-string))
      (set-frame-font default-font-string nil t)))
  (blink-cursor-mode -1))

(use-package help-fns
  :ensure nil
  :general
  (kdz/leader-help-def "M" '("Describe Mode" . describe-mode)))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode))

(use-package man
  :ensure nil
  :general
  (kdz/leader-help-def "m" '("Lookup Manpage" . man)))

(use-package minibuffer
  :ensure nil
  :config
  (setopt completion-cycle-threshold 1
          completions-detailed t))

(use-package mouse
  :ensure nil
  :general
  (kdz/leader-window-def "T" '("Tear off Window" . tear-off-window))
  :config
  (setopt mouse-yank-at-point t)
  (when (display-graphic-p) (context-menu-mode)))

(use-package mule
  :ensure nil
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package mule-util
  :ensure nil
  :config
  (setopt truncate-string-ellipsis "…"))

(use-package pixel-scroll
  :ensure nil
  :config
  (setopt pixel-scroll-precision-mode t
          pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-mode))

(use-package project
  :ensure nil
  :general
  (kdz/leader-def
    "p"   (cons "Project" (make-sparse-keymap))
    "pa" '("Add Projects" . project-remember-projects-under)
    "pD" '("Remove Project" . project-forget-project)
    "pe" '("Project Errors" . lsp-treemacs-errors-list)
    "pf" '("Open Project File" . project-find-file)
    "pp" '("Switch To Project" . project-switch-project))
  (kdz/leader-buffer-def
    "b" '("Switch to Buffer (Workspace)" . project-switch-to-buffer))

  :config
  (setq project-list-file (kdz/user-directory ".local" "projects"))

  (defvar kdz-project-switch-init-hook nil
    "Hooks to run after selecting a project via `project-switch-project'.

This is executed *prior* to running on of `project-switch-commands'.")

  (defvar kdz-project-switch-after-init-hook nil
    "Hooks to run after running one of `project-switch-commands'.")

  ;; Run this hook only after we've selected the project
  (advice-add 'project-switch-project
              :before
              (lambda (&rest _)
                (run-hooks 'kdz-project-switch-init-hook)))

  ;; Run this hook after we've selected/undertaken one of `project-switch-commands'
  (advice-add 'project-switch-project
              :after
              (lambda (&rest _)
                (run-hooks 'kdz-project-switch-after-init-hook))))

(use-package re-builder
  :ensure nil
  :general
  (kdz/leader-search-def "r" '("Regexp Builder" . re-builder)))

(use-package savehist
  :ensure nil
  :config
  (setopt savehist-file (kdz/user-directory ".local" "history"))
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :config
  (setopt save-place-file (kdz/user-directory ".local" "places"))
  (save-place-mode 1))

(use-package simple
  :ensure nil
  :general
  (kdz/leader-buffer-def "d" '("Kill Buffer" . kill-current-buffer))
  :config
  (setopt save-interprogram-paste-before-kill t)
  (setq-default indent-tabs-mode nil)
  (column-number-mode 1))

(use-package so-long
  :ensure nil
  :config
  (global-so-long-mode))

(use-package subword
  :ensure nil
  :general
  (kdz/leader-toggle-def "w" '("Sub-word mode" . subword-mode))
  (kdz/leader-toggle-global-def "w" '("Sub-word mode" . global-subword-mode)))

(use-package thingatpt
  :ensure nil
  :config
  (require 'lib/tap))

(use-package uniquify
  :ensure nil
  :config
  (setopt uniquify-buffer-name-style 'post-forward))

(use-package url
  :ensure nil
  :config
  (setq url-configuration-directory (kdz/user-directory ".local" "url")))

(use-package use-package-core
  :ensure nil
  :config
  (setopt use-package-enable-imenu-support t))

(use-package vc-hooks
  :ensure nil
  :config
  (setopt vc-follow-symlinks t))

(use-package which-key
  :ensure nil
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
        which-key-add-column-padding 2
	which-key-sort-uppercase-first nil
        which-key-max-description-length 30
        which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-prefix-prefix "✚ "
        which-key-show-prefix 'left)
  (which-key-mode))

(use-package window
  :ensure nil
  :general
  (kdz/leader-window-def "=" '("Balance Windows" . balance-windows))
  (kdz/leader-buffer-def
    "B" '("Switch to Buffer (Global)" . switch-to-buffer)
    "n" '("Next Buffer"               . next-buffer)
    "p" '("Previous Buffer"           . previous-buffer))
  
  :config
  (setopt switch-to-buffer-obey-display-actions t)

  (defmacro kdz/display-rule-buffer-to-tab (pattern tab-name)
    `(add-to-list 'display-buffer-alist
                  '(,pattern display-buffer-in-tab (tab-name . ,tab-name))))

  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer _)
                    (with-current-buffer buffer
                      (derived-mode-p 'comint-mode
                                      'flycheck-error-list-mode
                                      'xref--xref-buffer-mode
                                      'embark-collect-mode)))
                 display-buffer-in-side-window
                 (side . bottom)
                 (slot . 99)
                 (dedicated . t)))

  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer _)
                    (with-current-buffer buffer
                      (derived-mode-p 'help-mode 'helpful-mode)))
                 display-buffer-in-side-window
                 (side . right)
                 (dedicated . t)))

  (kdz/display-rule-buffer-to-tab "\\*dashboard\\*"       "Home")
  (kdz/display-rule-buffer-to-tab "\\*scratch\\*"         "Scratchpad")
  (kdz/display-rule-buffer-to-tab "\\*Ibuffer\\*"         "Buffers")
  (kdz/display-rule-buffer-to-tab "\\*Messages\\*"        "System")
  (kdz/display-rule-buffer-to-tab "\\*Warnings\\*"        "System")
  (kdz/display-rule-buffer-to-tab "\\*elfeed-log\\*"      "System")
  (kdz/display-rule-buffer-to-tab "\\*lsp-install: .+\\*" "System")
  (kdz/display-rule-buffer-to-tab "\\*Packages\\*"        "Packages")
  (kdz/display-rule-buffer-to-tab "\\*elpaca-log\\*"      "Packages"))

(use-package xref
  :ensure nil
  :general
  (kdz/leader-code-lookup-def
    "d" '("Lookup Definition" . xref-find-definitions)
    "r" '("Lookup References" . xref-find-references))
  :config
  (setq xref-prompt-for-identifier nil
        xref-search-program 'ripgrep))

(provide 'packages.d/emacs)
