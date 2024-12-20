(use-package tab-bar
  :hook (emacs-startup . kdz/tab-bar-initialize-tab-state)
  :config
  (defvar kdz-tab-bar-tab-icons '(("Home"       . "nf-md-home")
                                  ("Packages"   . "nf-md-package")
                                  ("Scratchpad" . "nf-md-note")
                                  ("System"     . "nf-md-cog"))
    "Tabs that should be kept together and in order in tab list")

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

  (defun kdz/tab-bar-render-pinned-tabs ()
    (let* ((all-tabs (funcall tab-bar-tabs-function))
           (relative-index (kdz/tab-bar-map-tabs-to-relative-index)))
      (mapcan (lambda (tab) (tab-bar--format-tab tab (alist-get tab relative-index)))
              (seq-filter #'kdz/tab-bar-pinned-tab-p all-tabs))))

  (defun kdz/tab-bar-render-workspaces ()
    (let* ((all-tabs (funcall tab-bar-tabs-function))
           (relative-index (kdz/tab-bar-map-tabs-to-relative-index)))
      (mapcan (lambda (tab) (tab-bar--format-tab tab (alist-get tab relative-index)))
              (seq-remove #'kdz/tab-bar-pinned-tab-p all-tabs))))

  (defun kdz/tab-bar-update-faces (&rest _)
    "Customize tab-bar faces against current theme

    This is performed via a function so it can be used as a hook on
    actions that would update colors in emacs (such as changing themes)"
    (let ((box-width 7)
          (box-style (if (version<= "30" emacs-version) 'flat-button 'flat))
          (background-color (face-background 'tab-bar nil t)))
      (set-face-attribute 'tab-bar
                          nil
                          :box (list :line-width box-width
                                     :color background-color
                                     :style box-style)
                          :underline (list :color 'foreground-color
                                           :position (* -1 box-width)))
      (set-face-attribute 'tab-bar-tab-inactive nil :weight 'normal)
      (set-face-attribute 'tab-bar-tab
                          nil
                          :background background-color
                          :foreground "selectedControlColor"
                          :weight 'bold
                          :underline (list :color (face-foreground 'tab-bar)
                                           :position (* -1 box-width)))
      (set-face-attribute 'tab-bar-tab-inactive nil :weight 'normal)))

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
        tab-bar-format '(tab-bar-separator
                         tab-bar-separator
                         kdz/tab-bar-render-workspaces
                         tab-bar-format-align-right
                         kdz/tab-bar-render-pinned-tabs
                         tab-bar-separator))
  (tab-bar-select-tab-by-name "Home")
  (tab-bar-close-tab-by-name "*scratch*"))

(use-package tab-line
  :config
  (setq tab-line-close-button-show nil
        tab-line-tab-name-truncated-max 40
        tab-line-new-button-show nil)

  (defvar kdz-tab-line-mode-icon-alist
    '((inferior-emacs-lisp-mode . "nf-custom-emacs")
      (inferior-python-mode     . "nf-md-language_python")
      (comint-mode              . "nf-dev-terminal")
      (tabulated-list-mode      . "nf-fa-list")))

  (defvar kdz-tab-line-mode-renaming-alist
    '((comint-mode         . kdz/tab-line-comint-name)
      (embark-collect-mode . kdz/tab-line-embark-name)))

  (defvar kdz-embark-collect-friendly-type-alist
    '((consult-grep . "Search")))

  (defun kdz/tab-line-icon-for-buffer (buffer)
    (with-current-buffer buffer
      (when-let ((icon-name (cdr (seq-find (lambda (mapping)
                                             (derived-mode-p (car mapping)))
                                           kdz-tab-line-mode-icon-alist))))
        (kdz/propertize-nerd-icon icon-name))))

  (defun kdz/tab-line-comint-name (buffer)
    (string-replace "*" "" (buffer-name buffer)))

  (defun kdz/tab-line-embark-name (buffer)
    (if-let* (
              (embark-type (with-current-buffer buffer embark--type))
              (embark-command (with-current-buffer buffer
                                (symbol-name embark--command)))
              (friendly-type-name (alist-get embark-type
                                             kdz-embark-collect-friendly-type-alist))
              (despecialized-name (stirng-replace "*" "" (buffer-name buffer)))
              (minibuffer-input (replace-regexp-in-string (concat "^.+ "
                                                                  embark-command
                                                                  " - ")
                                                          ""
                                                          despecialized-name)))
        (concat (or friendly-type-name (symbol-name embark-command))
                ": "
                minibuffer-input)))

  (defun kdz/tab-line-name-for-mode (buffer)
    (if-let* ((buffer-mode (with-current-buffer buffer major-mode))
              (name-fn-for-mode (alist-get buffer-mode
                                           kdz-tab-line-mode-renaming-alist)))
        (funcall name-fn-for-mode buffer)
      (buffer-name buffer))
    )

  (defun kdz/tab-line-buffer-display-name (buffer &optional _buffers)
    (or (kdz/tab-line-name-for-mode buffer) (buffer-name buffer)))


  (defun kdz/tab-line-tab-name-as-search-results (name)
    (replace-regexp-in-string "\\*Embark Collect: consult-ripgrep - #"
                              "Search: "
                              name))

  (defun kdz/tab-line-tab-name-format (tab tabs)
    (let* ((buffer-p (bufferp tab))
           (selected-p (if buffer-p
                           (eq tab (window-buffer))
                         (cdr (assq 'selected tab))))
           (name (if buffer-p
                     (funcall tab-line-tab-name-function tab tabs)
                   (cdr (assq 'name tab))))
           (icon (when buffer-p (kdz/tab-line-icon-for-buffer tab)))
           (face (if selected-p
                     (if (mode-line-window-selected-p)
                         'tab-line-tab-current
                       'tab-line-tab)
                   'tab-line-tab-inactive)))
      (dolist (fn tab-line-tab-face-functions)
        (setf face (funcall fn tab tabs face buffer-p selected-p)))
      (apply 'propertize
             (concat "[ "
                     icon
                     (when icon " ")
                     (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                                 'face face
                                 'keymap tab-line-tab-map
                                 'help-echo (if selected-p "Current tab"
                                              "Click to select tab")
                                 ;; Don't turn mouse-1 into mouse-2 (bug#49247)
                                 'follow-link 'ignore)
                     " ]")
             `(tab ,tab ,@(if selected-p '(selected t))))))

  (setq tab-line-tab-name-function #'kdz/tab-line-buffer-display-name
        tab-line-tab-name-format-function #'kdz/tab-line-tab-name-format)

  (advice-add #'tab-line-tab-name-buffer
              :filter-return
              (lambda (val) (concat " " val " ")))

  (advice-add #'tab-line-tab-name-buffer
              :filter-return #'kdz/tab-line-tab-name-as-search-results)

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
      (tab-line-mode 1)))

  (add-hook 'window-state-change-hook #'kdz/ensure-bottom-tab-line)

  (defun kdz/tab-line-tab-name (buffer &optional _buffers)
    (let* ((comint-mode-p (with-current-buffer buffer
                            (derived-mode-p 'comint-mode)))
           (icon (when comint-mode-p
                   `(" " ,(nerd-icons-devicon "nf-dev-terminal"))))
           (parts `(" [ " ,(buffer-name buffer) ,icon " ] ")))
      (apply 'concat (flatten-list parts))))

  (setq tab-line-tab-name-function #'kdz/tab-line-tab-name))

(provide 'packages.d/ui/tabs)
