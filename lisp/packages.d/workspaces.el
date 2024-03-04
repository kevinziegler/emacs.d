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

(use-package tab-bar
  :config
  (load "lib/tab-bar.el")

  ;; TODO Figure out why this isn't affecting the behavior for
  ;;      switching tabs correctly.  Once that's done, I *should*
  ;;      be able to use the partitioned tabs correctly.
  ;; (advice-add tab-bar-tabs-function
  ;;             :filter-return
  ;;             #'kdz/tab-bar-tabs-sort-pinned-tabs-last)

  (setq tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg
        tab-bar-new-tab-to 'rightmost
        tab-bar-format '(tab-bar-separator
                         kdz/tab-bar-format-project-icon
                         tab-bar-separator
                         kdz/tab-bar-format-unpinned-tabs
                         tab-bar-format-align-right
                         tab-bar-separator
                         kdz/tab-bar-format-pinned-tabs
                         tab-bar-separator
                         kdz/tab-bar-format-pin-icon)))

(use-package tabspaces
  :straight t
  :after dashboard
  :config
  (setq tabspaces-session-file (kdz/user-directory ".local"
                                                   "tabsession.el"))

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
