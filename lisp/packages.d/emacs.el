(use-package emacs
  :init
  (require 'uniquify)

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
          custom-file (kdz/user-directory ".local" "custom.el")
          auto-save-list-file-prefix (kdz/user-directory ".local" "auto-save-list/.saves-")
          backup-directory-alist (or backup-directory-alist
                                     `(("." . ,(kdz/user-directory ".local" "backups")))))
  (setopt appropos-do-all t
          auto-revert-avoid-polling t
          auto-revert-check-vc-info t
          auto-revert-interval 5
          backup-by-copying t
          catppuccin-flavor 'mocha
          column-number-mode t
          completion-cycle-threshold 1
          completions-detailed t
          cursor-in-non-selected-windows nil
          fill-column 80
          frame-inhibit-implied-resize t
          indicate-buffer-boundaries 'left
          mouse-yank-at-point t
          pixel-scroll-precision-mode t
          save-interprogram-paste-before-kill t
          ;; show-trailing-whitespace t
          truncate-string-ellipsis "…"
          uniquify-buffer-name-style 'post-forward
          use-package-enable-imenu-support t
          use-short-answers t
          x-stretch-cursor t
          x-underline-at-descent-line nil)

  (setq-default indent-tabs-mode nil)


  (savehist-mode 1)
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode)
  (global-auto-revert-mode)
  (global-hl-line-mode)
  (when (display-graphic-p) (context-menu-mode)))
