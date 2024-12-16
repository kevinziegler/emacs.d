(use-package hydra :straight t)

(use-package nerd-icons
  :straight t
  :config

  (defvar kdz-nerd-icons-function-map
    '(("nf-cod"     . nerd-icons-codicon)
      ("nf-dev"     . nerd-icons-devicon)
      ("nf-fa"      . nerd-icons-faicon)
      ("nf-iec"     . nerd-icons-ipsicon)
      ("nf-md"      . nerd-icons-mdicon)
      ("nf-oct"     . nerd-icons-octicon)
      ("nf-pom"     . nerd-icons-pomicon)
      ("nf-seti"    . nerd-icons-sucicon)
      ("nf-custom"  . nerd-icons-sucicon)
      ("nf-weather" . nerd-icons-wicon))
    "Mapping of nf-<iconset> names to their nerd-icons-* function ")

  (defvar kdz-nerd-icons-family-function-map
    '(("nf-cod"     . nerd-icons-codicon-family)
      ("nf-dev"     . nerd-icons-devicon-family)
      ("nf-fa"      . nerd-icons-faicon-family)
      ("nf-iec"     . nerd-icons-ipsicon-family)
      ("nf-md"      . nerd-icons-mdicon-family)
      ("nf-oct"     . nerd-icons-octicon-family)
      ("nf-pom"     . nerd-icons-pomicon-family)
      ("nf-seti"    . nerd-icons-sucicon-family)
      ("nf-custom"  . nerd-icons-sucicon-family)
      ("nf-weather" . nerd-icons-wicon-family))
    "Mapping of nf-<iconset> names to their nerd-icons-* function ")

  (defun kdz/nerd-icons-dwim (name)
    "Get a nerd-icons icon with name, regardless of icon set"
    (funcall (cdr (seq-find (lambda (mapping)
                              (s-starts-with? (car mapping) name))
                            kdz-nerd-icons-function-map))
             name))

  (defun kdz/propertize-nerd-icon (name &optional face-function)
    (let ((face-family (funcall (cdr (seq-find (lambda (mapping)
                                                 (s-starts-with? (car mapping) name))
                                               kdz-nerd-icons-family-function-map)) )))
      (propertize (kdz/nerd-icons-dwim name)
                  'face `(:family ,face-family :height 1.2)
                  'font-lock-face `(:family ,face-family :height 1.2)
                  'display '(raise 0)) ) ))

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

(use-package casual :straight t)

(provide 'packages.d/ui/base)
