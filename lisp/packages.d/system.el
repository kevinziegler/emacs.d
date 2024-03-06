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
