(use-package list-environment
  :straight t
  :general
  (kdz/leader-help-def "e" '("List Environment Variables" . list-environment)))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package terminal-here
  :straight t
  :general
  (kdz/leader-open-def "s" '("Terminal (Current Directory)" . terminal-here))
  :config
  (setq terminal-here-mac-terminal-command
        (lambda (path) (list "open" "-a" "iTerm.app" path))))

(use-package dired
  :straight nil
  :config
  (setq insert-directory-program "gls"))

;; TODO Figure out key binding for 'quit' - 'q' is currently bound to a generic
;;      'quit window' action, not
(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode))

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package logview :straight t)

(use-package reveal-in-osx-finder
  :straight t
  :general
  (kdz/leader-open-def
    "F" '("Finder Window (Current Directory)" . reveal-in-osx-finder)))

(use-package crux
  :straight t
  :general
  (kdz/leader-file-def
    "r" '("Rename File" . crux-rename-file-and-buffer)
    "D" '("Delete File" . crux-delete-file-and-buffer)
    "c" '("Copy File"   . crux-copy-file-preserve-attributes)))

(provide 'packages.d/system)
