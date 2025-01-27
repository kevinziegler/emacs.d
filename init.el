(add-to-list 'load-path (kdz/user-directory "lisp"))

(require 'packages.d/bootstrap)
(require 'packages.d/emacs)
(require 'packages.d/system)
(require 'packages.d/tools/help)
(require 'packages.d/evil)
(require 'packages.d/languages)
(require 'packages.d/org)
(require 'packages.d/workspaces)

(require 'packages.d/tools/dashboard)
(require 'packages.d/tools/completion)
(require 'packages.d/tools/lsp)

(require 'packages.d/ui/base)
(require 'packages.d/ui/buffer)
(require 'packages.d/ui/tabs)

(require 'lib/system)
(require 'lib/tap)
(require 'lib/window-resize)

(let* ((local-config (kdz/user-directory "local.el"))
       (local-template (concat local-config ".tpl")))
  (when (not (file-exists-p local-config))
    (copy-file local-template local-config))
  (require 'config-local local-config))
