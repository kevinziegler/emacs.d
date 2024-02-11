(set-frame-font "Berkeley Mono 12" nil t)

;;; Supply a hook for running updates after `load-theme'
(defvar kdz-load-theme-hook nil
  "Hook to run actions after calling `load-theme'")

(advice-add 'load-theme
            :after
            (lambda (&rest _) (run-hooks 'kdz-load-theme-hook)))

(advice-add 'stimmung-themes-load-light
            :after
            (lambda (&rest _) (run-hooks 'kdz-load-theme-hook)))

(advice-add 'stimmung-themes-load-dark
            :after
            (lambda (&rest _) (run-hooks 'kdz-load-theme-hook)))

(add-hook 'kdz-load-theme-hook 'kdz/set-hl-todo-faces)
(add-hook 'kdz-load-theme-hook 'kdz/tab-bar-update-faces)
