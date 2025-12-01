(defvar kdz-tab-line-mode-icon-alist
  '((inferior-emacs-lisp-mode . "nf-custom-emacs")
    (inferior-python-mode     . "nf-md-language_python")
    (comint-mode              . "nf-dev-terminal")
    (tabulated-list-mode      . "nf-fa-list")))

(defvar kdz-tab-line-mode-renaming-alist
  '((comint-mode . kdz/tab-line-comint-name)))

(defvar kdz-embark-type-friendly-names
  '((consult-grep . "Search")))

(defvar kdz-embark-collect-query-regexp-template
  "\\*Embark Collect: %s - #\\(.+\\)\\*$"
  "Regexp to extract the query for results in an embark-collect buffer.")

(defun kdz/embark-buffer-friendly-name (buffer-or-name)
  (with-current-buffer buffer-or-name
    (let* ((base (or (alist-get embark--type kdz-embark-type-friendly-names)
                     embark--command))
           (query-regexp (format kdz-embark-collect-query-regexp-template
                                 embark--command))
           (query (progn (string-match query-regexp) (match-string 1)))
           (buffer (when embark--target-buffer
                     (format " [%s]" embark--target-buffer))))
      (concat (format "%s: %s" base query) buffer))))

(defun kdz/embark-collect--friendly-name-search (buffer-or-name)
  (with-current-buffer buffer-or-name
    (let* ((query-regexp (format kdz-embark-collect-query-regexp-template
                                 embark--command))
           (query (progn (string-match query-regexp) (match-string 1)))))))

;; Variables I'm working with:
;; - embark--type :: Generic type for the collected results (e.g. consult-grep)
;; - embark--command :: Specific command the action (e.g. consult-ripgrep)
;; - embark--target-buffer :: Buffer that was the target of the consult action
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

(defun kdz/tab-line-tab-face-inactive (tab _tabs face _buffer-p selected-p)
  (let ((inherited (if selected-p 'tab-line-tab-current 'tab-line-tab-inactive)))
    (setf face `(:inherit (,inherited ,face))))
  face)

(defun kdz/window-move-dwim (tab-switch-fn window-switch-fn)
  (if (and (eq 'bottom (window-parameter nil 'window-side)) tab-line-mode)
      (funcall tab-switch-fn)
    (funcall window-switch-fn 1)))

(defun kdz/window-left-dwim ()
  (interactive)
  (kdz/window-move-dwim 'tab-line-switch-to-prev-tab 'evil-window-left))

(defun kdz/window-right-dwim ()
  (interactive)
  (kdz/window-move-dwim 'tab-line-switch-to-next-tab 'evil-window-right))

(defun kdz/tab-line-ensure-in-side-window (&rest args)
  (when (and (or (eq 'right  (window-parameter nil 'window-side))
                 (eq 'bottom (window-parameter nil 'window-side)))
             (not tab-line-mode))
    (tab-line-mode 1)))

(provide 'lib/tab-line-extras)
