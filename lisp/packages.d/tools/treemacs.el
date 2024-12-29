(use-package treemacs
  :general
  (general-def
    :keymaps 'treemacs-mode-map
    :prefix "o"
    "v" 'treemacs-visit-node-horizontal-split
    "h" 'treemacs-visit-node-vertical-split
    "s" 'treemacs-visit-node-vertical-split)

  (general-def
    :keymaps 'treemacs-mode-map
    :prefix "o a"
    "v" 'treemacs-visit-node-ace-horizontal-split
    "h" 'treemacs-visit-node-ace-vertical-split
    "s" 'treemacs-visit-node-ace-vertical-split)

  (kdz/leader-open-def "t" '("Project File Tree" . treemacs))

  :config
  (setq treemacs-collapse-dirs 7
        treemacs-width 45
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-project-follow-cleanup t)
  (treemacs-hide-gitignored-files-mode 1))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-tab-bar
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-evil :after (treemacs evil))

(provide 'packages.d/tools/treemacs)
