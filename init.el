(scroll-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode)
(set-frame-font "Berkeley Mono 12" nil t)

(defmacro kdz/init (path)
  `(load (expand-file-name ,path ,user-emacs-directory)))

;; Basic load order:
;; - First, load packages (packages.el) and initialize them (package-configs.el)
;; - Next, load custom function definitions, etc (lib)
;; - Lastly, apply any configurations that should happen after the environment is
;;   basicaly up-and-running (conf.d)

(kdz/init "packages.d/bootstrap.el")
(kdz/init "packages.d/evil.el")
(kdz/init "packages.d/languages.el")
(kdz/init "packages.d/lsp.el")
(kdz/init "packages.d/org.el")
(kdz/init "packages.d/utilities.el")

(kdz/init "lib/appearance.el")
(kdz/init "lib/misc-actions.el")
(kdz/init "lib/org.el")
(kdz/init "lib/tap.el")

(kdz/init "conf.d/appearance.el")
(kdz/init "conf.d/behaviors.el")
(kdz/init "conf.d/keybindings.el")
(kdz/init "conf.d/settings.el")
(kdz/init "conf.d/org.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" default))
 '(ignored-local-variable-values '((elisp-lint-indent-specs (when-let . 1)))))
