(use-package hydra :straight t)

(use-package nerd-icons
  :straight t
  :config
  (defun kdz/propertize-nerd-icon (name face-function)
    (propertize (nerd-icons-mdicon name)
                'face `(:family ,(funcall face-function) :height 1.2)
                'display '(raise 0)) ))

(use-package pretty-hydra :straight t)

(use-package transient
  :straight t
  :config
  (defun kdz/transient-path (file)
    (kdz/user-directory ".local" "transient" file))

  (setq transient-history-file (kdz/transient-path "history.el")
        transient-levels-file  (kdz/transient-path "levels.el")
        transient-values-file  (kdz/transient-path "values.el")))

(use-package avy :straight t)
(use-package ace-window
  :straight t
  :config
  (ace-window-posframe-mode)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0))

(provide 'packages.d/ui/base)
