(use-package tab-bar
  :ensure nil
  :after (custom nerd-icons)
  :general
  (kdz/leader-tab-def
    "TAB" '("Select Workspace"       . kdz/tab-switch-index-or-select)
    "h"   '("Previous Workspace"     . tab-previous)
    "l"   '("Next Workspace"         . tab-next)
    "H"   '("Move Tab Left"          . kdz/tab-move-left)
    "L"   '("Move Tab Right"         . tab-move)
    "d"   '("Close Workspace"        . tab-bar-close-tab)
    "n"   '("Create Named Workspace" . kdz/create-named-tab))
  :hook ((elpaca-after-init   . kdz/tab-bar-initialize-tab-state)
         (window-state-change . kdz/ensure-bottom-tab-line))
  :config
  (defvar kdz-tab-bar-tab-icons '(("Home"       . "nf-md-home")
                                  ("Packages"   . "nf-md-package")
                                  ("Scratchpad" . "nf-md-note")
                                  ("System"     . "nf-md-cog")
                                  ("Feeds"      . "nf-md-rss"))
    "Tabs that should be kept together and in order in tab list")

  (defun kdz/tab-move-left () (tab-move -1))

  (defvar kdz-blank-buffer-text  "Nothing to see here."
    "Filler text to use in *blank* buffer")

  (defvar kdz-blank-buffer-name  "*blank*"
    "Placeholder buffer name")

  ;; TODO Change the default directory for new tabs
  (defun kdz/create-named-tab ()
    (interactive)
    (tab-bar-new-tab)
    (tab-bar-rename-tab "*New Tab*")
    (kdz/show-blank-buffer)
    (call-interactively 'tab-bar-rename-tab))

  ;; TODO Make this center the content in the window
  (defun kdz/show-blank-buffer ()
    (interactive)
    (let ((blank-buffer (get-buffer-create kdz-blank-buffer-name)))
      (with-current-buffer blank-buffer
        (unless
            (save-excursion
              (beginning-of-buffer)
              (search-forward kdz-blank-buffer-text nil t))
          (insert kdz-blank-buffer-text))
        (hide-mode-line-mode 1))
      (switch-to-buffer blank-buffer)))

  (defun kdz/tab-bar-pinned-tab-p (tab)
    "Check if TAB should be considered a pinned tab in the tab bar

A pinned tab is one whose name corresponds to an entry in
`kdz-tab-bar-tab-icons'."
    (assoc (alist-get 'name tab) kdz-tab-bar-tab-icons))

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

  (defun kdz/tab-bar-map-tabs-to-relative-index ()
    (let ((unpinned-index 1)
          (pinned-index 9))
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
     (lambda (all-tabs) (seq-filter #'kdz/tab-bar-pinned-tab-p all-tabs))))

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

  (defun kdz/tab-bar-initialize-tab-state ()
    (tab-bar-select-tab-by-name "Home")
    (tab-bar-close-tab-by-name "*scratch*"))

  (setq tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format
        tab-bar-new-tab-to 'rightmost
        tab-bar-format '(kdz/tab-bar-format-workspaces
                         tab-bar-format-align-right
                         kdz/tab-bar-format-pinned-tabs)))

(use-package tab-line
  :ensure nil
  :after (custom nerd-icons)
  :config
  (setq tab-line-close-button-show nil
        tab-line-tab-name-truncated-max 40
        tab-line-new-button-show nil
        tab-line-tab-name-function #'kdz/tab-line-buffer-display-name
        tab-line-tab-name-format-function #'kdz/tab-line-tab-name-format)

  (defvar kdz-tab-line-mode-icon-alist
    '((inferior-emacs-lisp-mode . "nf-custom-emacs")
      (inferior-python-mode     . "nf-md-language_python")
      (comint-mode              . "nf-dev-terminal")
      (tabulated-list-mode      . "nf-fa-list")))

  (defvar kdz-tab-line-mode-renaming-alist
    '((comint-mode         . kdz/tab-line-comint-name)))

  (defvar kdz-embark-collect-friendly-type-alist
    '((consult-grep . "Search")))

  (defun kdz/tab-line-icon-name-for-buffer (buffer)
    (with-current-buffer buffer
      (cdr (seq-find (lambda (mapping) (derived-mode-p (car mapping)))
                     kdz-tab-line-mode-icon-alist))))

  (defun kdz/tab-line-comint-name (buffer)
    (string-replace "*" "" (buffer-name buffer)))

  (defun kdz/tab-line-name-for-mode (buffer)
    (if-let* ((buffer-mode (with-current-buffer buffer major-mode))
              (name-fn (cdr (seq-find (lambda (mapping)
                                        (derived-mode-p (car mapping)))
                                      kdz-tab-line-mode-renaming-alist))))
        (funcall name-fn buffer)
      (buffer-name buffer)))

  (defun kdz/tab-line-buffer-display-name (buffer &optional _buffers)
    (or (kdz/tab-line-name-for-mode buffer) (buffer-name buffer)))

  (defun kdz/tab-line-tab-name-format (tab tabs)
    (let* ((buffer-p (bufferp tab))
           (selected-p (if buffer-p
                           (eq tab (window-buffer))
                         (cdr (assq 'selected tab))))
           (name (if buffer-p
                     (funcall tab-line-tab-name-function tab tabs)
                   (cdr (assq 'name tab))))
           (icon (when buffer-p (kdz/tab-line-icon-name-for-buffer tab)))
           (face (if selected-p
                     (if (mode-line-window-selected-p)
                         'tab-line-tab-current
                       'tab-line-tab)
                   'tab-line-tab-inactive)))
      (dolist (fn tab-line-tab-face-functions)
        (setf face (funcall fn tab tabs face buffer-p selected-p)))

      (defun propertize-tab-line-string (string)
        (propertize string 'face face 'follow-link 'ignore))

      (apply 'propertize
             (concat (propertize-tab-line-string "[ ")
                     (when icon (kdz/propertize-nerd-icon icon `(face ,face)))
                     (when icon (propertize-tab-line-string " "))
                     (propertize-tab-line-string (string-replace "%" "%%" name))
                     (propertize-tab-line-string " ]"))
             `(tab ,tab ,@(if selected-p '(selected t))))))

  (defun kdz/window-left-dwim ()
    (interactive)
    (if (and (eq 'bottom (window-parameter nil 'window-side)) tab-line-mode)
        (tab-line-switch-to-prev-tab)
      (evil-window-left 1)))

  (defun kdz/window-right-dwim ()
    (interactive)
    (if (and (eq 'bottom (window-parameter nil 'window-side)) tab-line-mode)
        (tab-line-switch-to-next-tab)
      (evil-window-right 1)))

  (defun kdz/ensure-bottom-tab-line (&rest args)
    (when (and (eq 'bottom (window-parameter nil 'window-side))
               (not tab-line-mode))
      (tab-line-mode 1))))

(provide 'packages.d/ui/tabs)
