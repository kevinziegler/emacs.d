(require 'uniquify)

(defun kdz/local (path)
  (expand-file-name path
                    (expand-file-name ".local" user-emacs-directory)))

(setq backup-by-copying t
      frame-inhibit-implied-resize t
      pixel-scroll-precision-mode t
      show-trailing-whitespace t
      save-interprogram-paste-before-kill t
      appropos-do-all t
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      save-place-file (kdz/local "places")
      savehist-file (kdz/local "history")
      bookmark-file (kdz/local "bookmarks")
      custom-file (kdz/local "custom.el"))

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(kdz/local "backups")))))

(setq-default indent-tabs-mode nil)

(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin t)

(global-display-line-numbers-mode)
(savehist-mode 1)
(save-place-mode 1)


