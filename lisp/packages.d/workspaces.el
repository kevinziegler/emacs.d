(use-package project
  :ensure nil
  :general
  (kdz/leader-def
    "p"   (cons "Project" (make-sparse-keymap))
    "pa" '("Add Projects" . project-remember-projects-under)
    "pD" '("Remove Project" . project-forget-project)
    "pe" '("Project Errors" . lsp-treemacs-errors-list)
    "pf" '("Open Project File" . project-find-file)
    "pp" '("Switch To Project" . project-switch-project))
  (kdz/leader-buffer-def
    "b" '("Switch to Buffer (Workspace)" . project-switch-to-buffer))

  :config
  (setq project-list-file (kdz/user-directory ".local" "projects"))

  (defvar kdz-project-switch-init-hook nil
    "Hooks to run after selecting a project via `project-switch-project'.

This is executed *prior* to running on of `project-switch-commands'.")

  (defvar kdz-project-switch-after-init-hook nil
    "Hooks to run after running one of `project-switch-commands'.")

  ;; Run this hook only after we've selected the project
  (advice-add 'project-switch-project
              :before
              (lambda (&rest _)
                (run-hooks 'kdz-project-switch-init-hook)))

  ;; Run this hook after we've selected/undertaken one of `project-switch-commands'
  (advice-add 'project-switch-project
              :after
              (lambda (&rest _)
                (run-hooks 'kdz-project-switch-after-init-hook))))

(use-package project-rootfile
  :config
  (add-to-list 'project-rootfile-list ".project"))

(use-package ibuffer-project
  :hook (ibuffer . kdz/ibuffer-tune-sort-and-filter)
  :config
  (defun kdz/ibuffer-tune-sort-and-filter ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

(use-package otpp
  :after project
  :init
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  (otpp-mode 1)
  (otpp-override-mode 1))

(provide 'packages.d/workspaces)
