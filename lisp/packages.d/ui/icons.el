;;; -*- lexical-binding: t -*-
(use-package nerd-icons
  :after emacs
  :config
  (require 'lib/nerd-icons-extras))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook ((elpaca-after-init . nerd-icons-completion-mode)
         (marginalia-mode . nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-grep
  :init
  (nerd-icons-grep-mode)
  :custom
  ;; This setting is a pre-requirement, so an icon can be displayed near each
  ;; heading
  (grep-use-headings t))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon t))

(use-package nerd-icons-xref
  :hook (elpaca-after-init . nerd-icons-xref-mode))

(provide 'packages.d/ui/icons)
