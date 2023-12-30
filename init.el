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
(kdz/init "lib/tap.el")

(kdz/init "conf.d/appearance.el")
(kdz/init "conf.d/behaviors.el")
(kdz/init "conf.d/keybindings.el")
(kdz/init "conf.d/settings.el")
