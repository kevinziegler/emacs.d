(use-package project
  :config
  (setq project-list-file (kdz/user-directory ".local" "projects"))
  (defun kdz/project-dashboard-buffer (project-dir)
    "Generate a project-specific name for the dashboard buffer"
    (format "*dashboard: %s*" project-dir))

  (defun kdz/project-open-show-dashboard (project-dir)
    "Open a new dashboard buffer for the supplied PROJECT-DIR"
    (when (and project-dir (member (list project-dir) project--list))
      (let ((dashboard-buffer-name (kdz/project-dashboard-buffer project-dir))
            (dashboard-item-generators dashboard-item-generators)
            (dashboard-items '((ls-directories . 5)
                               (ls-files . 5))))
        (push `(project-status . ,(dashboard-project-status project-dir))
              dashboard-item-generators)
        (dashboard-open))))

  (defun kdz/project-kill-dashboard (project-dir)
    "Kill the dashboard buffer for the supplied PROJECT-DIR"
    (let ((dashboard (kdz/project-dashboard-buffer project-dir)))
      (when (get-buffer dashboard)
        (kill-buffer dashboard))))

  (advice-add 'project-switch-project
              :before
              #'kdz/project-open-show-dashboard)
  (advice-add 'project-switch-project
              :after
              #'kdz/project-kill-dashboard))

(use-package project-rootfile
  :straight t
  :config
  (add-to-list 'project-rootfile-list ".project"))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . kdz/ibuffer-tune-sort-and-filter)
  :config
  (defun kdz/ibuffer-tune-sort-and-filter ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

(use-package otpp
  :straight t
  :after project
  :init
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  (otpp-mode 1)
  (otpp-override-mode 1))

(provide 'packages.d/workspaces)
