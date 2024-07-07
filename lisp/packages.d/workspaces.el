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

(use-package tabspaces
  :straight t
  :after dashboard
  :config
  (setq tabspaces-session-file (kdz/user-directory ".local"
                                                   "tabsession.el"))

  (tabspaces-mode)
  (add-to-list 'tabspaces-exclude-buffers dashboard-buffer-name)
  (advice-add 'tabspaces-open-or-create-project-and-workspace
              :after
              (lambda (&rest _) (tabspaces-reset-buffer-list)))

  ;; Set this variable to skip buffers that wouldn't show up in the
  ;; current tab per tabspaces's rules, to avoid buffers "leaking"
  ;; into the current space from other spaces.
  (setq switch-to-prev-buffer-skip
        (lambda (window buffer bury-or-kill)
          (not (tabspaces--local-buffer-p buffer))))

  (defun kdz/create-named-tab (tab-name)
    "Create a named tab with a new scratch buffer"
    (interactive "sName for new workspace: ")
    (switch-to-buffer (generate-new-buffer (format "*scratch: %s*"
                                                   tab-name)))
    (tabspaces-switch-or-create-workspace tab-name))

  (tab-bar-select-tab-by-name "Home")
  (tab-bar-close-tab-by-name "*scratch*"))

(use-package ibuffer-project
  :straight t
  :config
  (add-hook 'ibuffer-hook
 	    (lambda ()
 	      (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups))
 	      (unless (eq ibuffer-sorting-mode 'project-file-relative)
 		(ibuffer-do-sort-by-project-file-relative))))
  (add-hook 'ibuffer-hook
 	    (lambda ()
 	      (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups)))))

(provide 'packages.d/workspaces)
