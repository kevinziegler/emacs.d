(use-package abridge-diff :after magit :hook (elpaca-after-init . abridge-diff-mode))
(use-package deflate :ensure (:host github :repo "skuro/deflate" :branch "main"))
(use-package dirvish :hook (elpaca-after-init . dirvish-override-dired-mode))
(use-package docker  :after transient :general (kdz/leader-open-def "d" '("Docker" . docker)))
(use-package gcmh    :demand t :hook (elpaca-after-init . gcmh-mode))
(use-package git-modes :mode (("/.dockerignore\\'" . gitignore-mode )))
(use-package list-environment :general (kdz/leader-help-def "e" '("ENV Variables" . list-environment)))
(use-package logview)
(use-package magit-filenotify :hook (after-save . magit-after-save-refresh-status))

(use-package transient
  :custom
  (transient-history-file (kdz/transient-path "history.el"))
  (transient-levels-file  (kdz/transient-path "levels.el"))
  (transient-values-file  (kdz/transient-path "values.el"))
  :init
  (defun kdz/transient-path (file) (kdz/user-directory ".local" "transient" file)))

(use-package crux
  :general
  (kdz/leader-file-def
    "r" '("Rename File" . crux-rename-file-and-buffer)
    "D" '("Delete File" . crux-delete-file-and-buffer)
    "c" '("Copy File"   . crux-copy-file-preserve-attributes)))

(use-package difftastic
  :after transient
  :if (executable-find "difft")
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

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
    (let ((git-link-use-commit t)) (call-interactively 'git-link)))

  (defun kdz/git-link--tag ()
    "Get the latest tag for constructing a git-link URL."
    (car (git-link--exec "describe" "--tags" "HEAD")))

  (advice-add #'git-link--branch :after-until #'kdz/git-link--tag))

(use-package git-timemachine
  :after transient
  :general (kdz/leader-git-def "t" '("Time Machine" . git-timemachine)))

(use-package magit
  :after transient
  :custom
  (magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0)))
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :general
  (kdz/leader-git-def
    "g" '("Git Status"   . magit-status)
    "b" '("Blame File"   . magit-blame)
    "l" '("Log for File" . magit-log-buffer-file))
  :config
  (transient-append-suffix 'magit-fetch "-p" '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull  "-r" '("-a" "Autostash" "--autostash"))

  (when-let ((brew-git (executable-find (brew-bin "git"))))
    (setq magit-git-executable brew-git)))

(use-package magit-delta
  :if (executable-find "delta")
  :hook ((magit-mode . magit-delta-mode))
  :config
  (defconst kdz-magit-delta-delta-args-default magit-delta-delta-args)
  (defconst kdz-magit-delta-delta-extra-args
    '("--features='side-by-side line-numbers decorations'" "--side-by-side"))

  (defun kdz/magit-delta-toggle-appearance ()
    (interactive)
    (if (eq kdz-magit-delta-delta-args-default magit-delta-delta-args)
        (setopt magit-delta-delta-args (append magit-delta-delta-args
                                               kdz-magit-delta-delta-extra-args))
      (setopt magit-delta-delta-args kdz-magit-delta-delta-args-default))))

(provide 'packages.d/system)
