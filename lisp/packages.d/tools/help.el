(use-package help-fns
  :straight nil
  :general
  (kdz/leader-help-def "M" '("Describe Mode" . describe-mode)))

(use-package helpful
  :straight t
  :general
  (kdz/leader-help-def
    "f" '("Describe Callable" . helpful-callable)
    "v" '("Describe Variable" . helpful-variable)
    "k" '("Describe Key"      . helpful-key)))

(use-package man
  :general
  (kdz/leader-help-def "m" '("Lookup Manpage" . man)))

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

(use-package keycast
  :straight t
  :general
  (kdz/leader-toggle-def "k" '("Keycast Display" . keycast-tab-bar-mode)))

(use-package devdocs
  :straight t
  :general
  (kdz/leader-help-def
    "d"   (cons "DevDocs" (make-sparse-keymap))
    "dd" '("Search in Documentation" . devdocs-search)
    "dp" '("Peruse Documentation" . devdocs-peruse)
    "di" '("Download Documentation Set" . devdocs-install))

  :config
  (setq devdocs-data-dir (kdz/user-directory ".local/devdocs-data")))

(use-package eldoc-box
  :straight t
  :config
  ;; TODO Make box display in the bottom right of the frame
  (setq eldoc-box-only-multi-line t
        eldoc-box-position-function #'eldoc-box--default-at-point-position-function)

  ;; TODO Run this as a hook when changing themes
  (defun kdz/eldoc-box-update-faces ()
    (set-face-attribute 'eldoc-box-border
                        nil
                        :background (face-foreground 'default))))

(provide 'packages.d/tools/help)
