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
	which-key-prefix-prefix "• ")
  (setq which-key-show-prefix 'top)
  (which-key-mode))

(use-package keycast :straight t)
