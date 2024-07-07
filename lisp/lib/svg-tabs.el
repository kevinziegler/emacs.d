(defvar kdz-tab-bar-tab-svg-radius 3)
(defvar kdz-tab-bar-tab-svg-scale 1.3)
(defvar kdz-tab-bar-tab-svg-margin 0)

(defvar kdz-tab-bar-pinned-tabs-alist '(("Home"   . "[material:home]")
                                        ("System" . "[material:cog]"))
  "Tabs that should be kept together and in order in tab list

The CAR should be the name of the pinned tab, and the CDR of the cell
should contain the text to pass to `svg-tag-make' when generating the
SVG for the pinned tab")

(defvar kdz-tab-bar--svg-cache nil
  "Cache of SVG data for tab-bar SVGs

Emacs seems to be fairly eager about calling functions that would
refresh the tab-bar.  This is good to maintain the UI, but means
lots of calls to regenerate SVGs for the tab bar, which are expensive.

This cache holds SVG data keyed by the text content of the SVG and any
properties that might be customized on a per-svg basis.  This allows
us to skip re-generating the SVG data if we already have a copy on
hand, even when the displayed SVGs have changed (e.g. to swap a tab's
SVGs between active and in-active states).")

(defun kdz/tab-bar--svg-get-or-create-cached (text props)
  "Return the SVG to represent TEXT with PROPS in the tab bar

If a cached copy of the SVG is available, that cached content will be
returned.  Otherwise, a new SVG will be created and stored in the
cache.  Cached SVGs are kept in `kdz-tab-bar--svg-cache'."
  (let* ((key (format "%s-%s" text props))
         (cached (assoc key kdz-tab-bar--svg-cache))
         (result (or (cdr cached)
                     (apply #'svg-tag-make
                            text
                            (kdz/tab-bar-svg-default-props props)))))
    (when (not cached)
      (push (cons key result) kdz-tab-bar--svg-cache))

    result))

(defun kdz/tab-bar-tab-width ()
  "Calculate the width to use  for a tab-bar tab SVG"
  (let ((width 0))
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
    width))

(defun eli/tab-bar-svg-padding (width string)
  "Calculate tab-bar SVG padding based on supplied STRING and WIDTH"
  (let* ((style (svg-lib-style-default--get))
         (margin (plist-get style :margin))
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (tag-width (- width (* margin txt-char-width)))
         (padding (- (/ tag-width txt-char-width) (length string))))
    padding))

(defun kdz/tab-bar-svg-default-props (&optional extra)
  "Return the default properties for generated tab-bar SVGs

These properties are used by `kdz/tab-bar-svg' to generate the SVGs
useed to represent both pinned and un-pinned tab-bar tabs."
  (map-merge 'plist
             (list :font-size (* 12 kdz-tab-bar-tab-svg-scale)
                   :font-family (ns-font-name (frame-parameter nil
                                                               'font))
                   :height kdz-tab-bar-tab-svg-scale
                   :radius kdz-tab-bar-tab-svg-radius)
             extra))

(defun kdz/tab-bar-tab-svg (svg-text fallback-text &rest svg-props)
  "Return FALLBACK-TEXT overlayed with an SVG of SVG-TEXT

If supplied arguments in SVG-PROPS are passed as plist arguments to
`svg-tag-make'.  Additional default properties passed to `svg-tag-make' are
generated from `kdz/tab-bar-svg-default-props'."
  (propertize fallback-text
              'display
              (kdz/tab-bar--svg-get-or-create-cached svg-text svg-props)))

(defun kdz/tab-bar-tab-name-format-svg (tab i)
  "Generate an SVG to represent a tab-bar TAB at position I"
  (let* ((name (alist-get 'name tab))
         (tab-number-text (number-to-string i))
         (tab-name-text (alist-get 'name tab))
         (number-padding 3)
         (number-segment (if tab-bar-tab-hints (format "%d " i) ""))
         (name-segment (alist-get 'name tab))
         (name (concat number-segment name-segment))
         (width (kdz/tab-bar-tab-width))
         (padding (if tab-bar-auto-width
                      (eli/tab-bar-svg-padding width name)
                    (plist-get (svg-lib-style-default--get) :padding))))

    (concat
     (kdz/tab-bar-tab-svg tab-number-text
                          tab-number-text
                          :inverse t
                          :padding number-padding
                          :crop-right t)
     (kdz/tab-bar-tab-svg tab-name-text
                          tab-name-text
                          :inverse (eq (car tab) 'current-tab)
                          :padding (- padding number-padding)
                          :margin kdz-tab-bar-tab-svg-margin
                          :crop-left t))))

(defun kdz/tab-bar-tab-name-format-pinned-svg (tab i)
  "Generate an SVG icon to represent a pinned TAB at position I"
  (let* ((name (alist-get 'name tab))
         (icon (cdr (assoc name kdz-tab-bar-pinned-tabs-alist)))
         (svg-text (format "%s %d" icon i))
         (fallback-text (format "%d %s" i name)))
    (kdz/tab-bar-tab-svg (format "%s %d" icon i)
                         (format "%d %s" i name)
                         :inverse (eq (car tab) 'current-tab)
                         :margin 0
                         :padding 3)))

(define-minor-mode svg-tabs-mode
  "Display tab-bar tabs using SVG images"
  :global t
  (if svg-tabs-mode
      (setq tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg)
    (progn
      (message "svg-tabs-mode activated!"))
    (progn
      (message "svg-tabs-mode deactivated!"))))
