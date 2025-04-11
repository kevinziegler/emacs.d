(defvar kdz-tab-bar-tab-icons '(("Home"       . "nf-md-home")
                                ("Packages"   . "nf-md-package")
                                ("Scratchpad" . "nf-md-note")
                                ("System"     . "nf-md-cog")
                                ("Feeds"      . "nf-md-rss"))
  "Tabs that should be kept together and in order in tab list")

(defun kdz/tab-bar-pinned-tab-p (tab)
  "Check if TAB should be considered a pinned tab in the tab bar

A pinned tab is one whose name appears as an entry in `kdz-tab-bar-tab-icons'."
  (assoc (alist-get 'name tab) kdz-tab-bar-tab-icons))

(defun kdz/tab-bar-map-tabs-to-relative-index ()
  (let ((unpinned-index 1)
        (pinned-index (length (tab-bar-tabs))))
    (mapcar (lambda (tab)
              (if (kdz/tab-bar-pinned-tab-p tab)
                  (let ((index pinned-index))
                    (setq pinned-index (1- pinned-index))
                    (cons tab index))
                (let ((index unpinned-index))
                  (setq unpinned-index (1+ unpinned-index))
                  (cons tab index))))
            (tab-bar-tabs))))

(defun kdz/tab-bar-map-relative-index-to-tabs ()
  (mapcar (lambda (tab-to-index) (cons (cdr tab-to-index) (car tab-to-index)))
          (kdz/tab-bar-map-tabs-to-relative-index)))

(defun kdz/tab-bar-format-with-relative-index (filter-tabs)
  (let* ((all-tabs (funcall tab-bar-tabs-function))
         (relative-index (kdz/tab-bar-map-tabs-to-relative-index)))
    (mapcan (lambda (tab)
              (tab-bar--format-tab tab (alist-get tab relative-index)))
            (funcall filter-tabs all-tabs))))

(defun kdz/tab-bar-format-pinned-tabs ()
  (kdz/tab-bar-format-with-relative-index
   (lambda (all-tabs)
     (seq-reverse (seq-filter #'kdz/tab-bar-pinned-tab-p all-tabs) ))))

(defun kdz/tab-bar-format-workspaces ()
  (kdz/tab-bar-format-with-relative-index
   (lambda (all-tabs) (seq-remove #'kdz/tab-bar-pinned-tab-p all-tabs))))

(defun kdz/tab-switch-index-or-select (&optional index)
  "Change tabs, optionally by INDEX using a prefix argument.

    The INDEX refers to the value displayed in the tab-bar's tab hint, and is then
    mapped to the correct sequential index in tab-bar-tabs"
  (interactive "N")
  (if (eq index nil)
      (call-interactively 'tab-switch)
    (let ((tab (alist-get index (kdz/tab-bar-map-relative-index-to-tabs))))
      (if tab
          (tab-bar-select-tab (1+ (seq-position (tab-bar-tabs) tab)))
        (message "No tab found at position [%d]" index)))))

(defun kdz/tab-bar-tab-name-format (tab i)
  (let* ((name (alist-get 'name tab))
         (icon (cdr (assoc (alist-get 'name tab) kdz-tab-bar-tab-icons)))
         (tab-face (funcall tab-bar-tab-face-function tab)))
    (concat (propertize (concat "[ "
                                (when tab-bar-tab-hints (format "%d " i)))
                        'face tab-face)
            (if icon
                (kdz/propertize-nerd-icon icon `(face (:inherit ,tab-face)))
              (propertize name 'face tab-face))
            (propertize " ]" 'face tab-face))))

(provide 'lib/pinned-tabs)
