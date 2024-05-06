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

(use-package eldoc-box :straight t)

(provide 'packages.d/tools/help)
