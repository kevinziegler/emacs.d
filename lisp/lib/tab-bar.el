(defvar kdz-tab-bar-tab-svg-radius 3)
(defvar kdz-tab-bar-tab-svg-scale 1.3)
(defvar kdz-tab-bar-tab-svg-margin 0)

(defvar kdz-tab-bar-pinned-tabs-alist '(("Home" . "[material:home]")
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

(defun kdz/tab-bar-update-faces (&rest _)
  "Customize tab-bar faces against current theme

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
  (set-face-attribute 'tab-bar nil
		      :inherit 'mode-line
		      :box (list :line-width 7
                                 :color (face-background 'default)))
  (set-face-attribute 'tab-bar-tab nil
		      :inherit 'mode-line
		      :underline (list :color (face-background 'match)
                                       :position -7))
  (set-face-attribute 'tab-bar-tab-inactive nil :inherit 'mode-line))

(defun kdz/tab-switch-index-or-select (&optional index)
  "Change tabs, optionally by index using a prefix argument"
  (interactive "P")
  (if (eq index nil)
      (call-interactively 'tab-switch)
    (tab-bar-select-tab index)))

(defun kdz/create-named-tab (tab-name)
  "Create a named tab with a new scratch buffer"
  (interactive "sName for new workspace: ")
  (switch-to-buffer (generate-new-buffer (format "*scratch: %s*"
                                                 tab-name)))
  (tabspaces-switch-or-create-workspace tab-name))


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

(defun kdz/tab-bar-pinned-tab-p (tab)
  "Check if TAB should be considered a pinned tab in the tab bar

A pinned tab is one whose name corresponds to an entry in
`kdz-tab-bar-pinned-tabs-alist'."
  (assoc (alist-get 'name tab) kdz-tab-bar-pinned-tabs-alist))

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
  (let* ((style svg-lib-style-default)
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
`svg-tag-make'.  Additional default properties passed to
`svg-tag-make' are generated from `kdz/tab-bar-svg-default-props'."
  (propertize fallback-text
              'display
              (kdz/tab-bar--svg-get-or-create-cached svg-text
                                                     svg-props)
              ;; (apply #'svg-tag-make
              ;;        svg-text
              ;;        (kdz/tab-bar-svg-default-props svg-props))
              ))

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
                    (plist-get svg-lib-style-default :padding))))

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

(defun kdz/tab-bar-tab-name-format-text (tab i)
  "Provide a custom format for tab-bar tabs as plain text"
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat " "
	     (if tab-bar-tab-hints (format "[%d] " i) "")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p
                                   'non-selected
                                 'selected)))
                      tab-bar-close-button)
                 ""))
     'face (funcall tab-bar-tab-face-function tab))))

(defun kdz/tab-bar-format-toggle ()
  "Toggle between using SVG and plain text tab-bar format functions

This is mostly for testing/debugging tab-bar functionality. It
provides a safe fall back (plain text) to make sure I can use tabs in
the UI, as well as a way to force an update to the tab bar contents to
refresh the display if needed."
  (interactive)
  (setq tab-bar-tab-name-format-function
        (if (eq tab-bar-tab-name-format-function
                'kdz/tab-bar-tab-name-format-svg)
            'kdz/tab-bar-tab-name-format-text
          'kdz/tab-bar-tab-name-format-svg)))

(defun kdz/expanded-width-of (elements)
  "Calculate the total length of the evaluated contents of ELEMENTS

ELEMENTS should be a sequence of items that can be invoked as function
calls that return sequences that can be evaluated and summed by their
`length'."
  (seq-reduce #'+ (seq-map #'length (seq-map #'funcall elements)) 0))

(defun kdz/tab-bar-width-left-of (element)
  "Calculate the width of tab-bar contents to the left of ELEMENT

ELEMENT should be a symbol in `tab-bar-format',  with the returned
width being calculated by evaluating and summing all elements
preceding ELEMENT in `tab-bar-format'."
  (kdz/expanded-width-of
   (seq-take-while (lambda (item) (not (eq item element)))
                   tab-bar-format)))

(defun kdz/tab-bar-width-right-of (element)
  "Calculate the width of tab-bar contents to the right of ELEMENT

ELEMENT should be a symbol in `tab-bar-format',  with the returned
width being calculated by evaluating and summing all elements
following ELEMENT in `tab-bar-format'."
  (kdz/expanded-width-of
   (cdr (seq-drop-while (lambda (item) (not (eq item element)))
                        tab-bar-format))))

(defun kdz/tab-bar-list-centering-width ()
  (let* ( (tab-width (kdz/tab-bar-tab-width))
          (tab-list-tabs-width (* (length (tab-bar-tabs) tab-width)))
          (tab-list))))

(defun kdz/tab-bar-spacer (side width)
  "Generate a spacer of width WIDTH for SIDE in the tab-bar

These spacers are used to center the tab-bar tabs list while
respecting the contents of the tab-bar to the left and right of the
tab list.  The WIDTH argument should be a width in characters to use
for the spacer.  SIDE is used to provide a symbol name for the spacer
similar to the separators generated in the default tab list"
  (list (intern (format "sep-%s" side))
        'menu-item
        (make-string width ?\ )
        'ignore))

(defun kdz/tab-bar-format-tabs-centered ()
  "Render the list of tab-bar tabs as SVGs centered in the tab-bar"
  (let* ((tab-width (kdz/tab-bar-tab-width))
         (tab-list (funcall tab-bar-tabs-function))
         (tab-separators (- (length tab-list) 1))
         (txt-char-width (window-font-width nil 'fixed-pitch))
         (tab-list-px (+ (* (length tab-list) tab-width)
                         (* tab-separators
                            (length (tab-bar-separator))
                            txt-char-width)))
         (centering-px-per-side (/ (- (frame-inner-width) tab-list-px)
                                   2))
         (centering-chars-per-side (/ centering-px-per-side
                                      txt-char-width))
         (left-content-length (kdz/tab-bar-width-left-of
                               'kdz/tab-bar-format-tabs-centered))
         (right-content-length (kdz/tab-bar-width-right-of
                                'kdz/tab-bar-format-tabs-centered))
         (left-padding (- centering-chars-per-side
                          left-content-length
                          1))
         (right-padding (- centering-chars-per-side
                           right-content-length)))
    (append (list (kdz/tab-bar-spacer "left" left-padding))
            (let ((i 0))
              (mapcan
               (lambda (tab)
                 (setq i (1+ i))
                 (tab-bar--format-tab tab i))
               tab-list))
            (list (kdz/tab-bar-spacer "right" right-padding)))))

(defun kdz/tab-bar-unpinned-tabs (tabs)
  "Return all `unpinned' tabs from TABS

`Pinned' tabs are defined in kdz-tab-bar-pinned-tabs-list"
  (seq-remove #'kdz/tab-bar-pinned-tab-p tabs))

(defun kdz/tab-bar-tabs-sort-pinned-tabs-last (original-tabs)
  "Advice to sort tab-bar tabs with pinned tabs indexed last"
  (let ((unpinned-tabs (seq-remove #'kdz/tab-bar-pinned-tab-p
                                   original-tabs))
        (pinned-tabs (kdz/tab-bar-pinned-tabs original-tabs)))
    (append unpinned-tabs pinned-tabs)))

(defun kdz/tab-bar-format-unpinned-tabs ()
  "Format all unpinned tabs for display in the tab-bar"
  (let* ((all-tabs (funcall tab-bar-tabs-function))
         (unpinned-tabs (kdz/tab-bar-unpinned-tabs all-tabs)))
    (kdz/tab-bar--format-tabs all-tabs unpinned-tabs)))

(defun kdz/tab-bar-format-pinned-tabs ()
  "Format all pinned tabs for display in the tab-bar"
  (let* ((all-tabs (funcall tab-bar-tabs-function))
         (indices (seq-map-indexed
                   (lambda (tab index) (cons tab (+ index 1)))
                   all-tabs))
         (pinned-tabs (kdz/tab-bar-pinned-tabs all-tabs)))
    (let ((tab-bar-tab-name-format-function
           #'kdz/tab-bar-tab-name-format-pinned-svg))
      (kdz/tab-bar--format-tabs all-tabs pinned-tabs))))

(defun kdz/tab-bar--format-tabs (all-tabs tabs)
  "Render TABS with indices derived from ALL-TABS"
  (let* ((indices (seq-map-indexed
                   (lambda (tab index) (cons tab (+ index 1)))
                   all-tabs)))
    (mapcan (lambda (tab)
              (tab-bar--format-tab tab
                                   (1+ (seq-position all-tabs tab))))
            tabs)))

(defun kdz/tab-bar-format-project-icon ()
  (propertize (nerd-icons-octicon "nf-oct-project")))

(defun kdz/tab-bar-format-pin-icon ()
  (propertize (nerd-icons-octicon "nf-oct-pin")))

(defun kdz/tab-bar-unpinned-tabs (tabs)
  "Return all `unpinned' tabs from TABS

Pinned tabs are defined in `kdz-tab-bar-pinned-tabs-alist'"
  (seq-remove #'kdz/tab-bar-pinned-tab-p tabs))

(defun kdz/tab-bar-pinned-tabs (tabs)
  "Return all `pinned' tabs from TABS

Pinned tabs are defined in `kdz-tab-bar-pinned-tabs-alist'"
  (seq-filter #'kdz/tab-bar-pinned-tab-p
              tabs))

(defun kdz/tab-bar-reset-formatting ()
  (interactive)
  (let ((format tab-bar-format))
    (setq kdz-tab-bar--svg-cache nil)
    (setq tab-bar-format '())
    (tab-bar-mode -1)
    (setq tab-bar-format format)
    (tab-bar-mode +1)))

(define-minor-mode svg-tabs-mode
  "Display tab-bar tabs using SVG images"
  :global t
  (if svg-tabs-mode
      (setq tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg)
    (progn
      (message "svg-tabs-mode activated!"))
    (progn
      (message "svg-tabs-mode deactivated!"))))
