;;; Git/Version Control Tooling
(use-package magit
  :straight t
  :after 'kdz/system
  :config
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  (setq magit-git-executable (brew-bin "git")
        magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0))))

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

(use-package git-link
  :straight t
  :config
  (defun kdz/git-link-with-commit ()
    (interactive)
    (let ((git-link-use-commit t))
      (call-interactively 'git-link)))

  (defun kdz/git-link--tag ()
    "Get the latest tag for constructing a git-link URL."
    (car (git-link--exec "describe" "--tags" "HEAD")))

  (advice-add #'git-link--branch :after-until #'kdz/git-link--tag))
