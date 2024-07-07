(defun kdz/tab-bar-pinned-tab-p (tab)
  "Check if TAB should be considered a pinned tab in the tab bar

A pinned tab is one whose name corresponds to an entry in
`kdz-tab-bar-pinned-tabs-alist'."
  (assoc (alist-get 'name tab) kdz-tab-bar-pinned-tabs-alist))

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

(provide 'lib/tab-bar)
