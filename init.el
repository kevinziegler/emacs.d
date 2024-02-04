(defmacro kdz/init (path)
  `(load (expand-file-name ,path ,user-emacs-directory)))

(defun kdz/local (path)
  (expand-file-name path
                    (expand-file-name ".local" user-emacs-directory)))

;; Basic load order:
;; - First, load packages (packages.el) and initialize them (package-configs.el)
;; - Next, load custom function definitions, etc (lib)
;; - Lastly, apply any configurations that should happen after the environment is
;;   basicaly up-and-running (conf.d)

(kdz/init "packages.d/emacs.el")
(kdz/init "packages.d/bootstrap.el")
(kdz/init "packages.d/evil.el")
(kdz/init "packages.d/languages.el")
(kdz/init "packages.d/org.el")

(kdz/init "packages.d/tools/dashboard.el")
(kdz/init "packages.d/tools/minibuffer.el")
(kdz/init "packages.d/tools/treemacs.el")
(kdz/init "packages.d/tools/completion.el")
(kdz/init "packages.d/tools/help.el")
(kdz/init "packages.d/tools/git.el")
(kdz/init "packages.d/tools/lsp.el")

(kdz/init "packages.d/ui/base.el")
(kdz/init "packages.d/ui/appearance.el")
(kdz/init "packages.d/ui/buffer.el")
(kdz/init "packages.d/workspaces.el")

(kdz/init "lib/misc-actions.el")
(kdz/init "lib/tap.el")

(kdz/init "conf.d/behaviors.el")
(kdz/init "conf.d/keybindings.el")
(kdz/init "conf.d/settings.el")
