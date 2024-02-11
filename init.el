(add-to-list 'load-path (kdz/user-directory "lisp"))

(load "packages.d/emacs.el")
(load "packages.d/bootstrap.el")
(load "packages.d/system.el")
(load "packages.d/evil.el")
(load "packages.d/languages.el")
(load "packages.d/org.el")
(load "packages.d/workspaces.el")

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

(load "lib/misc-actions.el")
(load "lib/tap.el")

(load "conf.d/behaviors.el")
(load "conf.d/keybindings.el")
(load "conf.d/settings.el")

(let* ((local-config (kdz/user-directory "local.el"))
       (local-template (concat local-config ".tpl")))
  (when (not (file-exists-p local-config) )
    (copy-file local-template local-config))
  (load local-config))
