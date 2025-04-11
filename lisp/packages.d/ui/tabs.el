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
  (require 'lib/pinned-tabs)

  (defun kdz/tab-move-left () (interactive) (tab-move -1))

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

  (defun kdz/tab-bar-initialize-tab-state ()
    (tab-bar-select-tab-by-name "Home")
    (tab-bar-close-tab-by-name "*scratch*"))

  (setopt tab-bar-auto-width nil
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
  (setopt tab-line-close-button-show nil
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
