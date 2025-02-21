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
  (advice-add 'load-theme
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook))))

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

(use-package flycheck
  :ensure nil
  :config
  (setopt flycheck-checker-error-threshold 10000))

(use-package frame
  :ensure nil
  :general
  (kdz/leader-open-def "f" '("New Frame" . make-frame))
  :config
  (let ((default-font-string  "Berkeley Mono 12"))
    (when (find-font (font-spec :name default-font-string))
      (set-frame-font default-font-string nil t)))
  (blink-cursor-mode -1))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode))

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

(use-package uniquify
  :ensure nil
  :config
  (setopt uniquify-buffer-name-style 'post-forward))

(use-package use-package-core
  :ensure nil
  :config
  (setopt use-package-enable-imenu-support t))

(use-package vc-hooks
  :ensure nil
  :config
  (setq vc-follow-symlinks nil))

(use-package window
  :ensure nil
  :general
  (kdz/leader-window-def "=" '("Balance Windows" . balance-windows))
  (kdz/leader-buffer-def
    "B" '("Switch to Buffer (Global)"               . switch-to-buffer)
    "c" '("Switch to Buffer (Force current window)" . kdz/switch-to-buffer-current-window)
    "n" '("Next Buffer"                             . next-buffer)
    "p" '("Previous Buffer"                         . previous-buffer))
  :config
  (defun kdz/switch-to-buffer-current-window ()
    (interactive)
    (let ((switch-to-buffer-obey-display-actions nil))
      (switch-to-buffer)))

  (setq switch-to-buffer-obey-display-actions t)
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

  (add-to-list 'display-buffer-alist
               '("\\*Ibuffer\\*"
                 display-buffer-in-tab
                 (tab-name . "Buffers")))

  (add-to-list 'display-buffer-alist
               '("\\*Messages\\*"
                 display-buffer-in-tab
                 (tab-name . "System")))

  (add-to-list 'display-buffer-alist
               '("\\*Warnings\\*"
                 display-buffer-in-tab
                 (tab-name . "System")))

  (add-to-list 'display-buffer-alist
               '("\\*lsp-install: .+\\*"
                 display-buffer-in-tab
                 (tab-name . "System")))

  (add-to-list 'display-buffer-alist
               '("\\*straight-process\\*"
                 display-buffer-in-tab
                 (tab-name . "Packages")))

  (add-to-list 'display-buffer-alist
               '("\\*Packages\\*"
                 display-buffer-in-tab
                 (tab-name . "Packages")))

  (add-to-list 'display-buffer-alist
               '("\\*dashboard\\*"
                 display-buffer-in-tab
                 (tab-name . "Home")))

  (add-to-list 'display-buffer-alist
               '("\\*scratch\\*"
                 display-buffer-in-tab
                 (tab-name . "Scratchpad"))))

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
