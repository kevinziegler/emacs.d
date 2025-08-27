(use-package eglot
  :ensure nil
  :hook ((prog-mode . eglot-ensure)
         (eglot-mode . sideline-mode))
  :general
  (kdz/leader-toggle-def "h" '("Inlay hints" . eglot-inlay-hints-mode))
  (kdz/leader-code-def "a" '("Code Actions" . eglot-code-actions))
  :config
  (setq-default
   eglot-workspace-configuration
   '(:basedpyright
     (:typeCheckingMode "recommended")
     :basedpyright.analysis
     (:diagnosticSeverityOverrides
      (:reportUnusedCallResult "none")
      :inlayHints (:callArgumentNames :json-false)
      :diagnosticMode "openFilesOnly")))

  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0)))

;; TODO Make a helper to look for symbol at point by default
(use-package consult-eglot)
(use-package consult-eglot-embark)
(use-package flycheck-eglot)

(use-package sideline-eglot
  :after sideline
  :init
  (add-to-list 'sideline-backends-right 'sideline-eglot))
;; (use-package projection)
;; (use-package projection-dape)
;; (use-package dape
;;   :general
;;   (kdz/leader-code-def "d" '("Debug" . dape)))

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config
  (setopt eglot-booster-io-only t)
  (eglot-booster-mode))

(provide 'packages.d/tools/eglot)
