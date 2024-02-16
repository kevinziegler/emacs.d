(use-package helpful :straight t)
(use-package general :straight t)

(use-package which-key
  :straight t
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-sort-uppercase-first nil
	which-key-prefix-prefix "☰ ")
  (which-key-mode))
