;;; Git/Version Control Tooling
(use-package magit :straight t)
(use-package git-timemachine :straight t)
(use-package abridge-diff :after magit :init (abridge-diff-mode 1))

(use-package magit-delta
  :straight t
  :hook ((magit-mode . magit-delta-mode)))

(use-package difftastic
  :straight t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package git-modes
  :straight t
  :mode (("/.dockerignore\\'" . gitignore-mode )))
