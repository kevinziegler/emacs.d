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

  (setopt user-full-name "Kevin Ziegler")

  ;;; Set paths for generated state files
  (setopt save-place-file (kdz/user-directory ".local" "places")
          savehist-file (kdz/user-directory ".local" "history")
          bookmark-file (kdz/user-directory ".local" "bookmarks")
          custom-file (kdz/user-directory ".local" "custom.el"))

  (let ((auto-save-dir (kdz/user-directory ".local" "auto-save"))
        (backup-dir (kdz/user-directory ".local" "backups")))
    (make-directory auto-save-dir t)
    (make-directory backup-dir t)
    (setf auto-save-list-file-prefix auto-save-dir
          auto-save-file-name-transforms
          `((".*" ,auto-save-dir t)))
    (make-directory backup-dir t)
    (setf backup-directory-alist
          `((".*" . ,backup-dir))))

  (setopt appropos-do-all t
          auto-revert-avoid-polling t
          auto-revert-check-vc-info t
          auto-revert-interval 5
          backup-by-copying t
          version-control t
          delete-old-versions t
          delete-by-moving-to-trash t
          kept-new-versions 5
          kept-old-versions 3
          echo-keystrokes 0.25
          ring-bell-function 'ignore
          catppuccin-flavor 'mocha
          column-number-mode t
          comint-prompt-read-only t
          completion-cycle-threshold 1
          completions-detailed t
          cursor-in-non-selected-windows nil
          fancy-splash-image (expand-file-name "logo.png" user-emacs-directory)
          fill-column 80
          frame-inhibit-implied-resize t
          indicate-buffer-boundaries 'left
          inhibit-compacting-font-caches t
          mouse-yank-at-point t
          pixel-scroll-precision-mode t
          pixel-scroll-precision-use-momentum t
          save-interprogram-paste-before-kill t
          ;; show-trailing-whitespace t
          truncate-string-ellipsis "…"
          uniquify-buffer-name-style 'post-forward
          use-package-enable-imenu-support t
          use-short-answers t
          x-stretch-cursor t
          x-underline-at-descent-line nil)

  (setq-default indent-tabs-mode nil)

  (setq-default history-length 1000
                prescient-history-length 1000)

  (savehist-mode 1)
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode)
  (global-auto-revert-mode)
  (global-hl-line-mode)
  (global-so-long-mode)
  (when (display-graphic-p) (context-menu-mode)))

(use-package eshell
  :init
  (setq eshell-directory-name (kdz/user-directory ".local" "eshell")))

(use-package uniquify)

(use-package window
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer _)
                    (with-current-buffer buffer
                      (derived-mode-p 'comint-mode)))
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
                 (tab-name . "Scratchpad")))

  (add-to-list 'display-buffer-alist
               '("\\*Embark Collect: .+\\*"
                 display-buffer-in-side-window
                 (side . bottom)
                 (slot . 99)
                 (dedicated . t))))

(provide 'packages.d/emacs)
