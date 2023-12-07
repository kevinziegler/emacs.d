(defun kdz/set-hl-todo-faces (&rest _)
  "Set face colors for hl-todo keywords"
  (setq hl-todo-keyword-faces
      `(("TODO"   . ,(face-foreground 'hl-todo))
        ("FIXME"  . ,(face-foreground 'ansi-color-red))
        ("DEBUG"  . ,(face-foreground 'ansi-color-cyan))
        ("NOTE"   . ,(face-foreground 'ansi-color-blue))
        ("STUB"   . ,(face-foreground 'ansi-color-green)))))

(defun kdz/tab-bar-update-faces (&rest _)
  "Customize tab-bar faces against current theme"
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
  "Provide a custom format for tab-bar tabs"
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

(defun kdz/vertico--format-candiate-marker-advice
    (orig cand prefix suffix index start)
  (setq cand (funcall orig cand prefix suffix index start))
  (concat (if (= vertico--index index)
              (propertize "» " 'face 'vertico-current)
            "  ")
          cand))
