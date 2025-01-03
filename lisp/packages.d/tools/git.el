;;; Git/Version Control Tooling
(use-package magit
  :general
  (kdz/leader-git-def
    "g" '("Git Status"   . magit-status)
    "b" '("Blame File"   . magit-blame)
    "l" '("Log for File" . magit-log-buffer-file))

  :config
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  (setq magit-git-executable (brew-bin "git")
        magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0))))

(use-package magit-file-icons
  :hook ((magit-mode . magit-file-icons-mode)))

(use-package git-timemachine
  :general
  (kdz/leader-git-def "t" '("Time Machine" . git-timemachine)))

(use-package abridge-diff :after magit :init (abridge-diff-mode 1))

(use-package magit-delta
  :if (executable-find "delta")
  :hook ((magit-mode . magit-delta-mode)))

(use-package magit-file-icons
  :after magit
  :init (magit-file-icons-mode 1))

(use-package difftastic
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package git-modes :mode (("/.dockerignore\\'" . gitignore-mode )))

(use-package git-link
  :general
  (kdz/leader-git-def
    "y"   (cons "Copy Link" (make-sparse-keymap))
    "yh" '("Repository Homepage" . git-link-homepage)
    "yy" '("File + Line Number" . git-link)
    "yY" '("File + Line Number (@ Commit)" . kdz/git-link-with-commit))

  :config
  (defun kdz/git-link-with-commit ()
    (interactive)
    (let ((git-link-use-commit t))
      (call-interactively 'git-link)))

  (defun kdz/git-link--tag ()
    "Get the latest tag for constructing a git-link URL."
    (car (git-link--exec "describe" "--tags" "HEAD")))

  (advice-add #'git-link--branch :after-until #'kdz/git-link--tag))

(provide 'packages.d/tools/git)
