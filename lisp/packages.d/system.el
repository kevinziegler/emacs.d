(use-package list-environment :straight t)

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package terminal-here
  :straight t
  :config
  (setq terminal-here-mac-terminal-command
        (lambda (path) (list "open" "-a" "iTerm.app" path))))
