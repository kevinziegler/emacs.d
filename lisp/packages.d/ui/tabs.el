(use-package tab-bar
  :config
  (defvar kdz-tab-bar-tab-icons '(("Home"   . "nf-md-home")
                                  ("System" . "nf-md-cog"))
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
      (concat (propertize (concat "["
                                  (when tab-bar-tab-hints (format "%d " i)))
                          'face tab-face)
              (if icon
                  (nerd-icons-mdicon icon)
                (propertize name 'face tab-face))
              (propertize "]" 'face tab-face))))

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
    (let ((box-width 7))
      (set-face-attribute 'tab-bar nil
                          :box (list :line-width box-width
                                     :color (face-background 'tab-bar nil t)
                                     :style 'flat)
                          :underline (list :color 'foreground-color
                                           :style 'line
                                           :position (* -1 box-width)))))

  (defun kdz/tab-bar-select-by-relative-index (&optional index)
    (let ((tab (alist-get index (kdz/tab-bar-map-relative-index-to-tabs))))
      (when tab (tab-bar-select-tab (1+ (seq-position (tab-bar-tabs) tab))))))

  (defun kdz/tab-switch-index-or-select (&optional index)
    "Change tabs, optionally by index using a prefix argument"
    (interactive "N")
    (if (eq index nil)
        (call-interactively 'tab-switch)
      (kdz/tab-bar-select-by-relative-index index)))

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
                         tab-bar-separator)))

(use-package tab-line
  :config
  (setq tab-line-close-button-show nil
        tab-line-tab-name-truncated-max 40
        tab-line-new-button-show nil)

  (defvar kdz-tab-line-pretty-name-alist
    '(("\\*Embark Collect: consult-ripgrep - #" . "Search: "))
    "Regexp replacements for tab-line tab names")

  (defun kdz/tab-line-tab-name-as-search-results (name)
    (replace-regexp-in-string "\\*Embark Collect: consult-ripgrep - #"
                              "Search: "
                              name))

  (advice-add #'tab-line-tab-name-buffer
              :filter-return
              (lambda (val) (concat " " val " ")))

  (advice-add #'tab-line-tab-name-buffer
              :filter-return #'kdz/tab-line-tab-name-as-search-results)

  (defun kdz/window-left-dwim ()
    (interactive)
    (if (and (eq 'bottom (window-parameter nil 'window-side))
             tab-line-mode)
        (when (not (eq (current-buffer)
                       (car (tab-line-tabs-window-buffers))))
          (tab-line-switch-to-prev-tab))
      (evil-window-left 1)))

  (defun kdz/window-right-dwim ()
    (interactive)
    (if (and (eq 'bottom (window-parameter nil 'window-side))
             tab-line-mode)
        (when (not (eq (current-buffer)
                       (car (last (tab-line-tabs-window-buffers)))))
          (tab-line-switch-to-next-tab))
      (evil-window-right 1)))

  (defun kdz/ensure-bottom-tab-line (&rest args)
    (when (and (eq 'bottom (window-parameter nil 'window-side))
               (not tab-line-mode))
      (tab-line-mode 1)))

  (add-hook 'window-state-change-hook #'kdz/ensure-bottom-tab-line)

  (defun kdz/tab-line-update-faces (&rest _)
    "Customize tab-bar faces against current theme

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (set-face-attribute 'tab-line nil
                        :inherit 'tab-bar
                        :box (list :line-width 7
                                   :color (face-background 'tab-line nil t)
                                   :style 'flat)
                        :underline (list :color 'foreground-color
                                         :style 'line
                                         :position -7))
    (set-face-attribute 'tab-line-tab nil :inherit 'tab-line
                        :box (list :line-width 7
                                   :color (face-background 'tab-line nil t)
                                   :style 'flat))
    (set-face-attribute 'tab-line-tab-current nil
                        :inherit 'tab-line-tab
                        :inverse-video t
                        :box (list :line-width 7
                                   :color (face-foreground 'tab-line-tab nil t)
                                   :style 'flat)
                        :underline (list :color (face-foreground 'tab-line nil t)
                                         :style 'line
                                         :position -7))))

(provide 'packages.d/ui/tabs)