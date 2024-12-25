(defvar bootstrap-version)
(setq straight-base-dir (kdz/user-directory ".local"))

(let ((bootstrap-file (kdz/user-directory ".local"
                                          "straight"
                                          "repos"
                                          "straight.el"
                                          "bootstrap.el"))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Set up `general' here so the `:general' keyword is available for use in
;; `use-package' declarations later in my config
(use-package general :straight t
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
    "TAB"  (cons "Workspace"       kdz-tab-actions-map)))

(provide 'packages.d/bootstrap)
