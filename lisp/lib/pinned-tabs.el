(defvar kdz-tab-bar-tab-icons '(("Home"          . "nf-md-home")
                                ("Packages"      . "nf-md-package")
                                ("Scratchpad"    . "nf-md-note")
                                ("Notes"         . "nf-md-notebook")
                                ("System"        . "nf-md-cog")
                                ("Configuration" . "nf-md-account_settings")
                                ("Feeds"         . "nf-md-rss"))
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

(defun kdz/tab-bar-format-with-relative-index (filter-tabs)
  (let* ((all-tabs (funcall tab-bar-tabs-function))
         (relative-index (kdz/tab-bar-map-tabs-to-relative-index)))
    (mapcan (lambda (tab) (tab-bar--format-tab tab (alist-get tab relative-index)))
            (funcall filter-tabs all-tabs))))

(defun kdz/tab-bar-format-pinned-tabs ()
  (kdz/tab-bar-format-with-relative-index
   (lambda (all-tabs) (seq-reverse (seq-filter #'kdz/tab-bar-pinned-tab-p all-tabs)))))

(defun kdz/tab-bar-format-workspaces ()
  (kdz/tab-bar-format-with-relative-index
   (lambda (all-tabs) (seq-remove #'kdz/tab-bar-pinned-tab-p all-tabs))))

(defun kdz/tab-bar-switch-to-index (index)
  (let* ((relative-index-map
          (mapcar (lambda (tab-to-index) (cons (cdr tab-to-index)
                                               (car tab-to-index)))
                  (kdz/tab-bar-map-tabs-to-relative-index)))
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

(defun kdz/tab-bar-tab-name-format (tab i)
  (let* ((tab-face   (funcall tab-bar-tab-face-function tab)))
    (s-replace "  " " "
               (format (propertize  "[ %d %s %s%s ]" 'face tab-face)
                       i
                       (kdz/tab-bar-tab-icon-segment tab tab-face)
                       (kdz/tab-bar-tab-name-segment tab tab-face)
                       (kdz/tab-bar-tab-git-segment  tab tab-face)))))

(defun kdz/tab-bar-tab-icon-segment (tab tab-face)
  (if-let* ((tab-name (alist-get 'name tab))
            (icon (cdr (assoc tab-name kdz-tab-bar-tab-icons))))
      (kdz/propertize-nerd-icon icon `(face (:inherit ,tab-face)))
    ""))

(defun kdz/tab-bar-tab-name-segment (tab tab-face)
  (if (and (not (eq (car tab) 'current-tab)) (kdz/tab-bar-pinned-tab-p tab))
      ""
    (alist-get 'name tab)))

(defun kdz/git-repo-name-for (dir)
  (when-let* ((file-directory-p (expand-file-name ".git" dir))
              (remote-url
               (shell-command-to-string
                (format "git -C %s config --get remote.origin.url " dir))))
    (f-base remote-url)))

(defun kdz/git-branch-name-for (dir)
  (when (file-directory-p (expand-file-name ".git" dir))
    (s-trim (shell-command-to-string (format "git -C %s rev-parse --abbrev-ref HEAD"
                                             dir)))))

(defun kdz/otpp-tab-git-branch (tab)
  (when-let ((tab-root (alist-get 'otpp-root-dir tab)))
    (kdz/git-branch-name-for tab-root)))

(defun kdz/otpp-tab-git-repo (tab)
  (when-let ((tab-root (alist-get 'otpp-root-dir tab)))
    (kdz/git-branch-name-for tab-root)))

(defun kdz/tab-bar-tab-git-segment (tab tab-face)
  (if-let ((branch-name (kdz/otpp-tab-git-branch tab)))
      (concat ":" (propertize branch-name 'face 'tab-bar))
    ""))

(provide 'lib/pinned-tabs)
