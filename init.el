(defun kdz/local (path)
  (expand-file-name path
                    (expand-file-name ".local" user-emacs-directory)))

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(load "packages.d/emacs.el")
(load "packages.d/bootstrap.el")
(load "packages.d/evil.el")
(load "packages.d/languages.el")
(load "packages.d/org.el")

(load "packages.d/tools/dashboard.el")
(load "packages.d/tools/minibuffer.el")
(load "packages.d/tools/treemacs.el")
(load "packages.d/tools/completion.el")
(load "packages.d/tools/help.el")
(load "packages.d/tools/git.el")
(load "packages.d/tools/lsp.el")

(load "packages.d/ui/base.el")
(load "packages.d/ui/appearance.el")
(load "packages.d/ui/buffer.el")
(load "packages.d/workspaces.el")

(load "lib/misc-actions.el")
(load "lib/tap.el")

(load "conf.d/behaviors.el")
(load "conf.d/keybindings.el")
(load "conf.d/settings.el")
