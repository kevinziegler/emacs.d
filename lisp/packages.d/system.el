(use-package list-environment
  :general
  (kdz/leader-help-def "e" '("List Environment Variables" . list-environment)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package terminal-here
  :general
  (kdz/leader-open-def "s" '("Terminal (Current Directory)" . terminal-here))
  :config
  (setq terminal-here-mac-terminal-command
        (lambda (path) (list "open" "-a" "iTerm.app" path))))

(use-package dired :ensure nil :config (setq insert-directory-program "gls"))

;; TODO Figure out key binding for 'quit' - 'q' is currently bound to a generic
;;      'quit window' action, not
(use-package dirvish :init (dirvish-override-dired-mode))
(use-package nerd-icons-dired :hook (dired-mode . nerd-icons-dired-mode))
(use-package logview)

(use-package reveal-in-osx-finder
  :general
  (kdz/leader-open-def
    "F" '("Finder Window (Current Directory)" . reveal-in-osx-finder)))

(use-package crux
  :general
  (kdz/leader-file-def
    "r" '("Rename File" . crux-rename-file-and-buffer)
    "D" '("Delete File" . crux-delete-file-and-buffer)
    "c" '("Copy File"   . crux-copy-file-preserve-attributes)))

(provide 'packages.d/system)
