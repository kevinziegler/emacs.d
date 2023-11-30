(defun kdz/tab-bar-update-faces (&rest _)
  (set-face-attribute 'tab-bar nil
		      :inherit 'mode-line
		      :box `(:line-width 7 :color ,(face-background 'tab-bar)))
  (set-face-attribute 'tab-bar-tab nil
		      :inherit 'mode-line
		      :height 1.0
		      :underline `(:color ,(face-background 'match) :position -7)
		      :foreground (face-foreground 'mode-line))
  (set-face-attribute 'tab-bar-tab-inactive nil
		      :inherit 'mode-line
		      :height 1.0
		      :foreground (face-foreground 'mode-line-inactive)))

(defun kdz/tab-bar-tab-name-format-svg (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat " "
	     (if tab-bar-tab-hints (format ":%d: " i) "")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p 'non-selected 'selected)))
                      tab-bar-close-button)
                 ""))
     'face (funcall tab-bar-tab-face-function tab))))

;; TODO Add this as a hook for catppuccin-reload
(kdz/tab-bar-update-faces)
(advice-add 'load-theme :after #'kdz/tab-bar-update-faces)

(setq tab-bar-close-button-show nil)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg)
