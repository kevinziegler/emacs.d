(use-package general
  :demand t
  :ensure (:wait t)
  :config
  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'kdz-leader-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer kdz/leader-def
    :keymaps 'kdz-leader-map)
  (kdz/leader-def "" nil)

  (general-create-definer kdz/mode-leader-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (kdz/mode-leader-def "" nil)

  (defmacro kdz/leader-nested-map-init (name)
    "Create a keymap and corresponding general definer based on NAME"
    `(progn
       (defvar-keymap ,(intern (format "kdz-%s-actions-map" name)))
       (general-create-definer ,(intern (format "kdz/leader-%s-def" name))
         :keymaps ',(intern (format "kdz-%s-actions-map" name)))
       (,(intern (format "kdz/leader-%s-def" name)) "" nil)))

  (kdz/leader-nested-map-init "buffer")
  (kdz/leader-nested-map-init "code")
  (kdz/leader-nested-map-init "code-eval")
  (kdz/leader-nested-map-init "code-lookup")
  (kdz/leader-nested-map-init "edit")
  (kdz/leader-nested-map-init "file")
  (kdz/leader-nested-map-init "git")
  (kdz/leader-nested-map-init "help")
  (kdz/leader-nested-map-init "insert")
  (kdz/leader-nested-map-init "open")
  (kdz/leader-nested-map-init "search")
  (kdz/leader-nested-map-init "tab")
  (kdz/leader-nested-map-init "toggle")
  (kdz/leader-nested-map-init "toggle-global")
  (kdz/leader-nested-map-init "window")
  (kdz/leader-nested-map-init "frame")

  (kdz/leader-code-def "e"   `("Code Evaluation" . ,kdz-code-eval-actions-map))
  (kdz/leader-code-def "l"   `("Lookup Symbols"  . ,kdz-code-lookup-actions-map))
  (kdz/leader-toggle-def "g" `("Toggle (Global)" . ,kdz-toggle-global-actions-map))

  (kdz/leader-def
    ;; Top-level actions
    "SPC" '("Execute Command"          . execute-extended-command)
    "RET" '("Show Context Menu"        . context-menu-open)
    "u"   '("Apply Universal Argument" . universal-argument)
    ";"   '("Evaluate Expression"      . pp-eval-expression)

    ;; Sub-leader menus
    "b"    (cons "Buffer"          kdz-buffer-actions-map)
    "c"    (cons "Code"            kdz-code-actions-map)
    "e"    (cons "Edit"            kdz-edit-actions-map)
    "f"    (cons "File"            kdz-file-actions-map)
    "g"    (cons "Git"             kdz-git-actions-map)
    "h"    (cons "Help & Info"     kdz-help-actions-map)
    "i"    (cons "Insert"          kdz-insert-actions-map)
    "o"    (cons "Open"            kdz-open-actions-map)
    "s"    (cons "Search"          kdz-search-actions-map)
    "t"    (cons "Toggle"          kdz-toggle-actions-map)
    "w"    (cons "Window"          kdz-window-actions-map)
    "W"    (cons "Frame"           kdz-frame-actions-map)
    "TAB"  (cons "Workspace"       kdz-tab-actions-map)))

(provide 'packages.d/general)
