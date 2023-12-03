(scroll-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode)
(set-frame-font "Berkeley Mono 12" nil t)
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 235))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(vertical-border-width . 0))

(setq user-init-path (file-name-directory user-init-file))
(defun kdz/load-init (relative-path)
  (load (expand-file-name relative-path user-init-path)))

(defmacro kdz/init (path)
  `(load (expand-file-name ,path ,user-init-path)))

(load  "~/.my.emacs.d/conf.d/tabbar.el")
(load  "~/.my.emacs.d/conf.d/vertico.el")

(load "~/.my.emacs.d/packages.el")
(load "~/.my.emacs.d/package-configs.el")

(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin t)

(load "~/.my.emacs.d/lib/misc-actions.el")
(load "~/.my.emacs.d/keybindings.el")
(load "~/.my.emacs.d/settings.el")

(global-display-line-numbers-mode)

;;; Start - org-modern lifted items
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" default)))
