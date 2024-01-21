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

(defun kdz/vertico--format-candiate-marker-advice
    (orig cand prefix suffix index start)
  (setq cand (funcall orig cand prefix suffix index start))
  (concat (if (= vertico--index index)
              (propertize "» " 'face 'vertico-current)
            "  ")
          cand))

(defun +evil-update-cursor-color-h ()
  "Update cursor colors to indicate evil state

The colors are based on face colors from the current theme; this
function is intended to run as a hook after changing Emacs' theme.

NOTE This function is lifted from Doom Emacs' evil module
"
  (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
  (put 'cursor 'evil-normal-color (face-background 'cursor)))

(defun +evil-default-cursor-fn ()
  "Get color to use for the cursor when in evil's `normal' state

NOTE This function is lifted from Doom Emacs' evil module"
  (evil-set-cursor-color (get 'cursor 'evil-normal-color)))

(defun +evil-emacs-cursor-fn ()
  "Get color to use for the cursor when in evil's `emacs' state

NOTE This function is lifted from Doom Emacs' evil module"
  (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

