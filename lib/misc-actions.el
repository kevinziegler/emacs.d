(defun kdz/toggle-line-numbers ()
  (interactive)
  (if display-line-numbers
      (setq display-line-numbers
	    (if (eq display-line-numbers 'relative) t 'relative))
    (message "Line numbers are currently disabled!")))

(defun kdz/tab-switch-index-or-select (&optional index)
  (interactive "P")
  (if (eq index nil)
      (call-interactively 'tab-switch)
    (tab-bar-select-tab index)))

(defun kdz/get-delete-options-plist (deleted-file)
  (when (file)
    (let ((confirmed (yes-or-no-p (format "Delete current file (File: %s)?" deleted-file)))
	  (kill-buffer (yes-or-no-p (format "Also kill buffer for %s?" deleted-file)))))
    (list :confirmed confirmed kill-buffer :kill-buffer)))

(defun kdz/delete-file ()
  (interactive)
  (let* ((this-file (f-this-file))
	 (options (kdz/get-delete-options-plist this-file)))
      (if (this-file)
	  (progn
	    (f-delete this-file))
	(message "No current file! (Buffer: %s)" (buffer-name)))))

(defun kdz/create-named-tab (tab-name)
  (interactive "sName for new tab: ")
  (tab-bar-new-tab)
  (switch-to-buffer (generate-new-buffer (format "*scratch: %s*"
                                                 tab-name)))
  (tab-bar-rename-tab tab-name))

(defun kdz/org-return-handle-point-at-heading ()
  "Handle <return> behavior when point is at an org heading"
  (let ((heading-start (org-entry-beginning-position)))
    (goto-char (org-entry-end-position))
    (cond ((and (org-at-heading-p)
                (= heading-start (org-entry-beginning-position)))
           ;; Entry ends on its heading; add newline after
           (end-of-line)
           (insert "\n\n"))
          (t
           ;; Entry ends after its heading; back up
           (forward-line -1)
           (end-of-line)
           (when (org-at-heading-p)
             ;; At the same heading
             (forward-line)
             (insert "\n")
             (forward-line -1))
           (while (not (looking-back "\\(?:[[:blank:]]?\n\\)\\{3\\}" nil))
             (insert "\n"))
           (forward-line -1)))))

(defun kdz/org-return-handle-point-at-checkbox ()
  "Handle <return> behavior when point is at a checkbox item"
  (if (string-match-p "^[[:space:]]*[-+] \\[ \\][[:space:]]*$"
                      (thing-at-point 'line))
      ;; Last list item was empty; remove it and insert an empty line
      (progn
        (beginning-of-line)
        (kill-line)
        (insert "\n"))
    (org-insert-todo-heading nil)))

(defun kdz/org-return-handle-point-in-item ()
  "Handle <return> behavior when point is inside a list item"
  (let ((context (org-element-context)))
    (if (or (eq 'plain-list (car context))  ; First item in list
            (and (eq 'item (car context))
                 (not (eq (org-element-property :contents-begin context)
                          (org-element-property :contents-end context))))
            (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
        ;; Non-empty item: Add new item.
        (org-insert-item)
      ;; Empty item: Close the list.
      ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
      (delete-region (line-beginning-position) (line-end-position))
      (insert "\n"))))

(defun kdz/org-return-handle-point-inside-table ()
  "Handle <return> behavior when point is inside a table"
  (cond ((save-excursion
           (beginning-of-line)
           ;; See `org-table-next-field'.
           (cl-loop with end = (line-end-position)
                    for cell = (org-element-table-cell-parser)
                    always (equal (org-element-property :contents-begin cell)
                                  (org-element-property :contents-end cell))
                    while (re-search-forward "|" end t)))
         ;; Empty row: end the table.
         (delete-region (line-beginning-position) (line-end-position))
         (org-return t))
        ;; Non-empty row: call `org-return-indent'.
        (t (org-return t))))

;;;###autoload
(defun kdz/org-return-dwim (&optional default)
  "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Lifted and modified from Sacha Chua's Emacs configuration:
  ;; https://sachachua.com/dotemacs/index.html
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/

  (interactive "P")
  (if default
      (org-return t)
    (cond
     ((org-at-heading-p) (kdz/org-return-handle-point-at-heading))
     ((org-at-item-checkbox-p) (kdz/org-return-handle-point-at-checkbox))
     ((org-in-item-p) (kdz/org-return-handle-point-in-item))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return t))
     ((org-at-table-p) (kdz/org-return-handle-point-inside-table))
     ;; All other cases: call `org-return-indent'.
     (t (org-return t)))))
