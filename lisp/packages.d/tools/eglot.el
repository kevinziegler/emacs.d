(use-package eglot
  :ensure nil
  :general
  (kdz/leader-toggle-def "h" '("Inlay hints" . eglot-inlay-hints-mode))
  (kdz/leader-code-def "a" '("Code Actions" . eglot-code-actions))
  :config
  (setq eglot-autoshutdown t))

;; TODO Make a helper to look for symbol at point by default
(use-package consult-eglot)
(use-package consult-eglot-embark)
(use-package flycheck-eglot)

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flycheck)))

;; (use-package projection)
;; (use-package projection-dape)
;; (use-package dape
;;   :general
;;   (kdz/leader-code-def "d" '("Debug" . dape)))
