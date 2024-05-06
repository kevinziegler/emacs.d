(use-package dashboard
  :straight t
  :after nerd-icons
  :init
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (setq dashboard-items '((projects . 5) (bookmarks . 5))
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-projects-backend 'project-el
        dashboard-projects-switch-function
	#'tabspaces-open-or-create-project-and-workspace)

  (dashboard-setup-startup-hook))

(use-package dashboard-ls
  :after dashboard
  :straight t
  :config
  (add-to-list 'dashboard-heading-icons
               '(ls-directories . "nf-oct-file_directory"))
  (add-to-list 'dashboard-heading-icons
               '(ls-files . "nf-oct-file")))

(use-package dashboard-project-status :after dashboard :straight t)

(provide 'packages.d/tools/dashboard)
