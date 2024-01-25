(require 'uniquify)

(defun kdz/local (path)
  (expand-file-name path
                    (expand-file-name ".local" user-emacs-directory)))

(set-frame-font "Berkeley Mono 12" nil t)

(setopt user-full-name "Kevin Ziegler")

;;; Set paths for generated state files
(setopt save-place-file (kdz/local "places")
        savehist-file (kdz/local "history")
        bookmark-file (kdz/local "bookmarks")
        custom-file (kdz/local "custom.el")
        backup-directory-alist (or backup-directory-alist
                                   `(("." . ,(kdz/local "backups")))))

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

(setq-default indent-tabs-mode nil)

;;; Supply a hook for running updates after `load-theme'
(defvar kdz-load-theme-hook nil
  "Hook to run actions after calling `load-theme'")

(advice-add 'load-theme
            :after
            (lambda (&rest _) (run-hooks 'kdz-load-theme-hook)))

(add-hook 'kdz-load-theme-hook 'kdz/set-hl-todo-faces)
(add-hook 'kdz-load-theme-hook 'kdz/tab-bar-update-faces)
(add-hook 'kdz-load-theme-hook '+evil-update-cursor-color-h)

;;; Load a nice looking theme
(load-theme 'creamsody t)

;;; Global Modes
(tab-bar-mode)
(global-display-line-numbers-mode)
(savehist-mode 1)
(save-place-mode 1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode)
(global-auto-revert-mode)
(global-hl-line-mode)
(when (display-graphic-p) (context-menu-mode))
