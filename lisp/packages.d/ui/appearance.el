(use-package catppuccin-theme :straight t)
(use-package creamsody-theme :straight t)
(use-package kanagawa-theme :straight t)
(use-package tao-theme :straight t)
(use-package stimmung-themes :straight t
  ;; :config (stimmung-themes-load-light)
  )

(use-package kaolin-themes
  :straight t
  :config
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(use-package spacious-padding
  :straight t
  :config
  (setq spacious-padding-widths (list :internal-border-width 7
                                      :header-line-width 4
                                      :mode-line-width 6
                                      :tab-width 4
                                      :right-divider-width 7
                                      :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package solaire-mode
  :straight t
  :after catppuccin-theme
  :config
  (solaire-global-mode +1))

(use-package svg-tag-mode :straight t)

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
	doom-modeline-persp-icon nil
	doom-modeline-persp-name nil
	doom-modeline-buffer-encoding nil))

(provide 'packages.d/ui/appearance)
