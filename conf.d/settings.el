(require 'uniquify)

(setq backup-by-copying t
      frame-inhibit-implied-resize t
      pixel-scroll-precision-mode t
      show-trailing-whitespace t
      save-interprogram-paste-before-kill t
      appropos-do-all t
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      save-place-file (expand-file-name ".local/places"
                                        user-emacs-directory)
      savehist-file (expand-file-name ".local/history"
                                        user-emacs-directory)
      custom-file (expand-file-name ".local/custom.el"
				    user-emacs-directory))

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
						 ".local/backups")))))

(setq-default indent-tabs-mode nil)

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin t)

(global-display-line-numbers-mode)
(savehist-mode 1)
(save-place-mode 1)


