(use-package helpful :straight t)
(use-package general :straight t)

(use-package which-key
  :straight t
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-sort-uppercase-first nil
        which-key-max-description-length 30
        which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
	which-key-prefix-prefix "• ")
  (setq which-key-show-prefix 'top)
  (which-key-mode))

(use-package keycast :straight t)

(use-package devdocs
  :straight t
  :config
  (setq devdocs-data-dir (kdz/user-directory ".local/devdocs-data")))

(use-package eldoc-box
  :straight t
  :config
  (setq eldoc-box-only-multi-line t)

  ;; TODO Make box display in the bottom right of the frame
  (setq eldoc-box-position-function #'eldoc-box--default-at-point-position-function)

  ;; TODO Run this as a hook when changing themes
  (defun kdz/eldoc-box-update-faces ()
    (let ((body-fg (face-foreground 'default)))
      (set-face-attribute 'eldoc-box-border nil :background body-fg))))

(provide 'packages.d/tools/help)
