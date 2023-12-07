(kdz/set-hl-todo-faces)
(kdz/tab-bar-update-faces)

;; TODO Add this as a hook for catppuccin-reload
(advice-add 'load-theme :after #'kdz/hl-todo-keyword-faces)
(advice-add 'load-theme :after #'kdz/tab-bar-update-faces)

(setq tab-bar-close-button-show nil)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg)
