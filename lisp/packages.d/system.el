(use-package abridge-diff :after magit :init (abridge-diff-mode 1))

(use-package crux
  :general
  (kdz/leader-file-def
    "r" '("Rename File" . crux-rename-file-and-buffer)
    "D" '("Delete File" . crux-delete-file-and-buffer)
    "c" '("Copy File"   . crux-copy-file-preserve-attributes)))

(use-package difftastic
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package dired :ensure nil :config (setq insert-directory-program "gls"))

(use-package dirvish :init (dirvish-override-dired-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package git-modes :mode (("/.dockerignore\\'" . gitignore-mode )))

(use-package git-timemachine
  :general
  (kdz/leader-git-def "t" '("Time Machine" . git-timemachine)))

(use-package list-environment
  :general
  (kdz/leader-help-def "e" '("List Environment Variables" . list-environment)))

(use-package logview)

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
  (when-let ((brew-git (executable-find (brew-bin "git"))))
    (setq magit-git-executable brew-git))

  (setq magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0))
        magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit-delta
  :if (executable-find "delta")
  :hook ((magit-mode . magit-delta-mode)))

(use-package magit-filenotify
  :hook (after-save . magit-after-save-refresh-status))

(use-package reveal-in-osx-finder
  :general
  (kdz/leader-open-def
    "F" '("Finder Window (Current Directory)" . reveal-in-osx-finder)))

(use-package terminal-here
  :general
  (kdz/leader-open-def "s" '("Terminal (Current Directory)" . terminal-here))
  :config
  (setq terminal-here-mac-terminal-command
        (lambda (path) (list "open" "-a" "iTerm.app" path))))

(use-package gcmh
  :demand t
  :config
  (add-hook 'elpaca-after-init-hook (lambda () (gcmh-mode 1))))

(use-package docker :general (kdz/leader-open-def "d" '("Docker" . docker)))

(provide 'packages.d/system)
