(use-package treemacs
  :straight t
  :defer t
  :config
  (setq treemacs-collapse-dirs 7
        treemacs-width 45
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-project-follow-cleanup t)
  (treemacs-hide-gitignored-files-mode 1))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-tab-bar
  :straight t
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil :straight t :after (treemacs evil))
