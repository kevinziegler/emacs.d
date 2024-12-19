(use-package emacs
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
                prescient-history-length 1000))

(use-package apropos
  :config
  (setopt appropos-do-all t))

(use-package autorevert
  :config
  (setopt auto-revert-avoid-polling t
          auto-revert-check-vc-info t
          auto-revert-interval 5)
  (global-auto-revert-mode 1))

(use-package bookmark
  :config
  (setopt bookmark-file (kdz/user-directory ".local" "bookmarks")))

(use-package cus-edit
  :config
  (setopt custom-file (kdz/user-directory ".local" "custom.el")))

(use-package comint
  :config
  (setopt comint-prompt-read-only t))

(use-package eshell
  :init
  (setq eshell-directory-name (kdz/user-directory ".local" "eshell")))

(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package minibuffer
  :config
  (setopt completion-cycle-threshold 1
          completions-detailed t))

(use-package mouse
  :config
  (setopt mouse-yank-at-point t)
  (when (display-graphic-p) (context-menu-mode)))

(use-package mule
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package mule-util
  :config
  (setopt truncate-string-ellipsis "…"))

(use-package pixel-scroll
  :config
  (setopt pixel-scroll-precision-mode t
          pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-mode))

(use-package savehist
  :config
  (setopt savehist-file (kdz/user-directory ".local" "history"))
  (savehist-mode 1))

(use-package saveplace
  :config
  (setopt save-place-file (kdz/user-directory ".local" "places"))
  (save-place-mode 1))

(use-package simple
  :config
  (setopt save-interprogram-paste-before-kill t)
  (setq-default indent-tabs-mode nil)
  (column-number-mode 1))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package uniquify
  :config
  (setopt uniquify-buffer-name-style 'post-forward))

(use-package use-package-core
  :config
  (setopt use-package-enable-imenu-support t))

(use-package window
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer _)
                    (with-current-buffer buffer
                      (derived-mode-p 'comint-mode
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

(provide 'packages.d/emacs)
