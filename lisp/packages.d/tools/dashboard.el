(use-package dashboard
  :after nerd-icons
  :init
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (setq dashboard-banner-official-png (expand-file-name "logo.png"
                                                        user-emacs-directory)
        dashboard-items '((projects . 5) (bookmarks . 5))
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-projects-backend 'project-el
        dashboard-projects-switch-function #'project-switch-project)

  (dashboard-setup-startup-hook))

(use-package dashboard-ls
  :after dashboard
  :config
  (add-to-list 'dashboard-heading-icons
               '(ls-directories . "nf-oct-file_directory"))
  (add-to-list 'dashboard-heading-icons
               '(ls-files . "nf-oct-file")))

(use-package dashboard-project-status :after dashboard)

(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
        elfeed-feeds (quote
                      (("https://www.reddit.com/r/commandline.rss" reddit commandline)
                       ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                       ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("https://hackaday.com/blog/feed/" hackaday linux)
                       ("https://news.ycombinator.com/rss" hackernews)))))

(provide 'packages.d/tools/dashboard)
