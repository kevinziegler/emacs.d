(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0))
  :hook ((prog-mode . kdz/eglot-ensure-maybe)
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

  (defun kdz/eglot-ensure-maybe ()
    (when (seq-contains-p
           (-flatten (mapcar #'car eglot-server-programs))
           major-mode)
      (eglot-ensure))))

;; TODO Make a helper to look for symbol at point by default
(use-package consult-eglot)
(use-package consult-eglot-embark)
(use-package flycheck-eglot)

(use-package sideline-eglot
  :after sideline
  :init (add-to-list 'sideline-backends-right 'sideline-eglot))

;; (use-package projection)
;; (use-package projection-dape)
;; (use-package dape :general (kdz/leader-code-def "d" '("Debug" . dape)))

(use-package eglot-booster
  :after eglot
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :hook (elpaca-after-init . eglot-booster-mode)
  :if (executable-find "emacs-lsp-booster")
  :custom (eglot-booster-io-only t))

(use-package eglot-header-line
  :ensure (eglot-header-line :host github :repo "soerlemans/eglot-header-line")
  :after eglot
  :hook (eglot-managed-mode . eglot-header-line-mode))
(provide 'packages.d/tools/eglot)
