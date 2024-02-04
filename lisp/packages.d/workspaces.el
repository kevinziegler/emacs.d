(use-package project
  :config
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
  :straight nil
  :config
  (load "lib/tab-bar.el")

  ;; TODO Figure out why this isn't affecting the behavior for
  ;;      switching tabs correctly.  Once that's done, I *should*
  ;;      be able to use the partitioned tabs correctly.
  ;; (advice-add tab-bar-tabs-function
  ;;             :filter-return
  ;;             #'kdz/tab-bar-tabs-sort-pinned-tabs-last)
  (defun kdz/tab-bar-update-faces (&rest _)
    "Customize tab-bar faces against current theme

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (set-face-attribute 'tab-bar nil
		        :inherit 'mode-line
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)
		        :box `(:line-width 7 :color ,(face-background 'default)))
    (set-face-attribute 'tab-bar-tab nil
		        :inherit 'mode-line
		        :height 1.0
		        :underline `(:color ,(face-background 'match) :position -7)
		        :foreground (face-foreground 'mode-line))
    (set-face-attribute 'tab-bar-tab-inactive nil
		        :inherit 'mode-line
		        :height 1.0
		        :foreground (face-foreground 'mode-line-inactive)))

  (defun kdz/tab-switch-index-or-select (&optional index)
    "Change tabs, optionally by index using a prefix argument"
    (interactive "P")
    (if (eq index nil)
        (call-interactively 'tab-switch)
      (tab-bar-select-tab index)))

  (defun kdz/create-named-tab (tab-name)
    "Create a named tab with a new scratch buffer"
    (interactive "sName for new tab: ")
    (tab-bar-new-tab)
    (switch-to-buffer (generate-new-buffer (format "*scratch: %s*"
                                                   tab-name)))
    (tab-bar-rename-tab tab-name))

  (declare-function kdz/tab-bar-tab-name-format-svg "init")

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
                         kdz/tab-bar-format-pin-icon))
  (tab-bar-mode))

(use-package tabspaces
  :straight t
  :config
  (setq tabspaces-session-file (expand-file-name ".local/tabsession.el"
						 user-emacs-directory))
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
