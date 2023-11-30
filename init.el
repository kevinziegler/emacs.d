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

(defun kdz-mli ()
  (nerd-icons-icon-for-mode major-mode))

(defun kdz-mode-line-mode-icon ()
  (nerd-icons-icon-for-mode major-mode))

(defun kdz-mode-line-buffer-status-icon ()
  (nerd-icons-mdicon (kdz-mode-line-buffer-status--nf-icon)
		     :face
		     'nerd-icons-blue))


(defun kdz-mode-line-buffer-status--nf-icon ()
  (if buffer-read-only
      "nf-md-content_save_off"
    (if (not (buffer-live-p (current-buffer)))
	"nf-md-content_save_alert"
      (if (buffer-modified-p)
	  "nf-md-content_save"
	"nf-md-content_save_check"))))

(setq mode-line-format
      (list "%e"
	    'mode-line-front-space
	    '(:eval (kdz-mli))
	    " "
	    '(:eval (kdz-mode-line-buffer-status-icon))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" default)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fringe ((t :background "White")))
;;  '(header-line ((t :box (:line-width 4 :color "grey90" :style nil))))
;;  '(header-line-highlight ((t :box (:color "Black"))))
;;  '(line-number ((t :background "White")))
;;  '(mode-line ((t :box (:line-width 6 :color "grey75" :style nil))))
;;  '(mode-line-active ((t :box (:line-width 6 :color "grey75" :style nil))))
;;  '(mode-line-highlight ((t :box (:color "Black"))))
;;  '(mode-line-inactive ((t :box (:line-width 6 :color "grey90" :style nil))))
;;  '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
;;  '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
;;  '(window-divider ((t :background "White" :foreground "White")))
;;  '(window-divider-first-pixel ((t :background "White" :foreground "White")))
;;  '(window-divider-last-pixel ((t :background "White" :foreground "White"))))

