;;; -*- lexical-binding: t -*-
(defvar kdz-tab-bar-tab-icons '(("Home"          . "nf-md-home")
                                ("Packages"      . "nf-md-package")
                                ("Scratchpad"    . "nf-md-note")
                                ("Notes"         . "nf-md-notebook")
                                ("System"        . "nf-md-cog")
                                ("Configuration" . "nf-md-account_settings")
                                ("Feeds"         . "nf-md-rss"))
  "Tabs that should be kept together and in order in tab list")

(defun kdz/tab-bar-system-tab-p (tab)
  "Check if TAB should be considered a system tab in the tab bar

A system tab is one whose name appears as an entry in `kdz-tab-bar-tab-icons'."
  (assoc (alist-get 'name tab) kdz-tab-bar-tab-icons))

(defun kdz/tab-bar-partition-tabs ()
  (seq-group-by (lambda (tab)
                  (if (kdz/tab-bar-system-tab-p tab) 'system 'workspace))
                (funcall tab-bar-tabs-function)))

(defun kdz/tab-bar-partitioned-index (partitioned-tabs)
  (let ((max-index (length (seq-mapcat #'cdr partitioned-tabs))))
    (append
     (seq-map-indexed (lambda (tab index) (cons tab (+ 1 index)))
                      (alist-get 'workspace partitioned-tabs))
     (seq-map-indexed (lambda (tab index) (cons tab (- max-index index)))
                      (alist-get 'system partitioned-tabs)))))

(defun kdz/tab-bar-format-partition (partition)
  (let* ((partitions (kdz/tab-bar-partition-tabs))
         (index (kdz/tab-bar-partitioned-index partitions)))
    (mapcan (lambda (tab) (tab-bar--format-tab tab (alist-get tab index)))
            (alist-get partition partitions))))

(defun kdz/tab-bar-format-system-tabs ()
  (kdz/tab-bar-format-partition 'system))

(defun kdz/tab-bar-format-workspaces ()
  (kdz/tab-bar-format-partition 'workspace))

(defun kdz/tab-bar-switch-to-index (index)
  (let* ((relative-index-map
          (mapcar (lambda (tab-to-index) (cons (cdr tab-to-index)
                                               (car tab-to-index)))
                  (kdz/tab-bar-partitioned-index (kdz/tab-bar-partition-tabs))))
         (tab (alist-get index relative-index-map)))
    (if tab
        (tab-bar-select-tab (1+ (seq-position (tab-bar-tabs) tab)))
      (message "No tab found at position [%d]" index))))

(defun kdz/tab-switch-index-or-select (&optional index)
  "Change tabs, optionally by INDEX using a prefix argument.

    The INDEX refers to the value displayed in the tab-bar's tab hint, and is then
    mapped to the correct sequential index in tab-bar-tabs"
  (interactive "P")
  (cond ((null index) (call-interactively 'tab-switch))
        ((numberp index) (kdz/tab-bar-switch-to-index index))
        (t (message "Expected a numeric index, received: %s" index))))

(provide 'lib/tab-bar-partitions)
