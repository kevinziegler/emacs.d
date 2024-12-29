(use-package catppuccin-theme
  :after custom
  :hook (elpaca-after-init . (lambda () (catppuccin-load-flavor 'mocha)))
  :config
  (advice-add 'catppuccin-reload
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook))))

(use-package creamsody-theme)
(use-package tao-theme)
(use-package stimmung-themes
  :config
  ;; (stimmung-themes-load-light)
  (advice-add 'stimmung-themes-load-light
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook)))

  (advice-add 'stimmung-themes-load-dark
              :after
              (lambda (&rest _) (run-hooks 'kdz-load-theme-hook))))

(use-package kaolin-themes
  :config
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths (list :internal-border-width 7
                                      :header-line-width 4
                                      :mode-line-width 6
                                      :tab-width 4
                                      :right-divider-width 7
                                      :scroll-bar-width 0))
  (spacious-padding-mode))

(use-package solaire-mode
  :after catppuccin-theme
  :config
  (solaire-global-mode +1))

(use-package svg-tag-mode)

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-time nil
	doom-modeline-persp-icon nil
	doom-modeline-persp-name nil
	doom-modeline-buffer-encoding nil))

(provide 'packages.d/ui/appearance)
