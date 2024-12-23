(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package company :straight t)
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode)

  ;; https://github.com/emacs-evil/evil-collection/issues/766
  (advice-remove 'corfu--setup 'evil-normalize-keymaps)
  (advice-remove 'corfu--teardown 'evil-normalize-keymaps)

  (advice-add 'corfu--setup
              :after (lambda (&rest r) (evil-normalize-keymaps)))
  (advice-add 'corfu--teardown
              :after (lambda (&rest r) (evil-normalize-keymaps))))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package hotfuzz :straight t
  :config
  (add-to-list 'completion-styles 'hotfuzz))

(provide 'packages.d/tools/completion)
