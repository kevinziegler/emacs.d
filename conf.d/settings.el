(require 'uniquify)

(defun kdz/local (path)
  (expand-file-name path
                    (expand-file-name ".local" user-emacs-directory)))

(setopt user-full-name "Kevin Ziegler")

(setopt save-place-file (kdz/local "places")
        savehist-file (kdz/local "history")
        bookmark-file (kdz/local "bookmarks")
        custom-file (kdz/local "custom.el"))

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(kdz/local "backups")))))

(setq-default indent-tabs-mode nil)

(setopt appropos-do-all t
        auto-revert-avoid-polling t
        auto-revert-check-vc-info t
        auto-revert-interval 5
        backup-by-copying t
        catppuccin-flavor 'mocha
        column-number-mode t
        completion-cycle-threshold 1
        completions-detailed t
        frame-inhibit-implied-resize t
        indicate-buffer-boundaries 'left
        mouse-yank-at-point t
        pixel-scroll-precision-mode t
        save-interprogram-paste-before-kill t
        show-trailing-whitespace t
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg
        uniquify-buffer-name-style 'forward
        x-underline-at-descent-line nil)

(kdz/set-hl-todo-faces)
(kdz/tab-bar-update-faces)

;; TODO Add this as a hook for catppuccin-reload
(advice-add 'load-theme :after #'kdz/set-hl-todo-faces)
(advice-add 'load-theme :after #'kdz/tab-bar-update-faces)

(load-theme 'catppuccin t)

(tab-bar-mode)
(global-display-line-numbers-mode)
(savehist-mode 1)
(save-place-mode 1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode)
(global-auto-revert-mode)
(global-hl-line-mode)
(when (display-graphic-p) (context-menu-mode))
