(defvar kdz-tab-bar-tab-svg-radius 3)
(defvar kdz-tab-bar-tab-svg-scale 1.3)
(defvar kdz-tab-bar-tab-svg-margin 0)

(defun kdz/tab-bar-tab-width ()
  (setq width 0)
  (when tab-bar-auto-width
    (setq width (/ (frame-inner-width)
                   (length (funcall tab-bar-tabs-function))))
    (when tab-bar-auto-width-min
      (setq width (max width (if (window-system)
                                 (nth 0 tab-bar-auto-width-min)
                               (nth 1 tab-bar-auto-width-min)))))
    (when tab-bar-auto-width-max
      (setq width (min width (if (window-system)
                                 (nth 0 tab-bar-auto-width-max)
                               (nth 1 tab-bar-auto-width-max))))))
  width)

(defun eli/tab-bar-svg-padding (width string)
  (let* ((style svg-lib-style-default)
         (margin (plist-get style :margin))
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (tag-width (- width (* margin txt-char-width)))
         (padding (- (/ tag-width txt-char-width) (length string))))
    padding))

(defun kdz/tab-bar-tab-name-format-svg (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (number-padding 3)
         (number-segment (if tab-bar-tab-hints (format "%d " i) ""))
         (name-segment (alist-get 'name tab))
         (name (concat number-segment name-segment))
         (padding (plist-get svg-lib-style-default :padding))
         (width (kdz/tab-bar-tab-width)))
    (when tab-bar-auto-width
      (setq padding (eli/tab-bar-svg-padding width name)))
    (concat
     (kdz/tab-bar-tab-svg-segment (number-to-string i)
                                  t
                                  number-padding
                                  :crop-right)
     (kdz/tab-bar-tab-svg-segment name-segment
                                  (eq (car tab) 'current-tab)
                                  (- padding number-padding)
                                  :crop-left))))

(defun kdz/tab-bar-tab-svg-segment (text inverse padding crop &optional icon)
  (let ((foreground-attr (if inverse :background :foreground))
        (background-attr (if inverse :foreground :background)))
    (propertize
     text
     'display
     (svg-tag-make (concat icon text)
                   crop t
                   :font-size (* 12 kdz-tab-bar-tab-svg-scale)
                   :font-family (ns-font-name (frame-parameter nil 'font))
                   :height kdz-tab-bar-tab-svg-scale
                   :inverse inverse
                   :margin kdz-tab-bar-tab-svg-margin
                   :padding (if icon (- padding (window-font-width nil 'fixed-pitch)) padding)
                   :radius kdz-tab-bar-tab-svg-radius))))

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

(defun kdz/tab-bar-tab-name-format-text (tab i)
  "Provide a custom format for tab-bar tabs"
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat " "
	     (if tab-bar-tab-hints (format "[%d] " i) "")
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

(defun kdz/tab-bar-format-toggle ()
  (interactive)
  (setq tab-bar-tab-name-format-function
        (if (eq tab-bar-tab-name-format-function
                'kdz/tab-bar-tab-name-format-svg)
            'kdz/tab-bar-tab-name-format-text
          'kdz/tab-bar-tab-name-format-svg) ))

(defun kdz/tab-bar-left-elements-size ()
  (let* ((elements (seq-take-while
                    (lambda (item)
                      (not (eq item 'kdz/tab-bar-format-tabs-centered)))
                    tab-bar-format))
         (expanded (seq-map #'funcall elements))
         (sizes (seq-map #'length expanded)))
    (seq-reduce #'+ sizes 0)))

(defun kdz/tab-bar-right-elements-size ()
  (let* ((elements (cdr (seq-drop-while
                         (lambda (item)
                           (not (eq item 'kdz/tab-bar-format-tabs-centered)))
                         tab-bar-format) ))
         (expanded (seq-map #'funcall elements))
         (sizes (seq-map #'length expanded)))
    (seq-reduce #'+ sizes 0)))

(defun kdz/tab-bar-list-centering-width ()
  (let* ( (tab-width (kdz/tab-bar-tab-width))
          (tab-list-tabs-width (* (length (tab-bar-tabs) tab-width)))
          (tab-list))))

(defun kdz/tab-bar-spacer (side width-in-chars)
  (list (intern (format "sep-%s" side))
        'menu-item
        (make-string width-in-chars ?\ )
        'ignore))

(defun kdz/tab-bar-format-tabs-centered ()
  (let* ((tab-width (kdz/tab-bar-tab-width))
         (tab-list-tabs-width (* (length (tab-bar-tabs)) tab-width))
         (tab-list (funcall tab-bar-tabs-function))
         (tab-separators (- (length tab-list) 1))
         (tab-separator-length (length (tab-bar-separator)))
         (txt-char-width (window-font-width nil 'fixed-pitch))
         (tab-list-px (+ (* (length tab-list) tab-width)
                         (* tab-separators
                            (length (tab-bar-separator))
                            txt-char-width)))
         (centering-px-per-side (/ (- (frame-inner-width) tab-list-px)
                                   2))
         (centering-chars-per-side (/ centering-px-per-side
                                      txt-char-width))
         (left-content-length (kdz/tab-bar-left-elements-size))
         (right-content-length (kdz/tab-bar-right-elements-size))
         (left-padding (- centering-chars-per-side left-content-length 1))
         (right-padding (- centering-chars-per-side right-content-length)))
    (append (list (kdz/tab-bar-spacer "left" left-padding))
            (let ((i 0))
              (mapcan
               (lambda (tab)
                 (setq i (1+ i))
                 (tab-bar--format-tab tab i))
               tab-list))
            (list (kdz/tab-bar-spacer "right" right-padding))
            )
    ))

(setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-format '(tab-bar-format-history kdz/tab-bar-format-tabs-centered tab-bar-separator))
