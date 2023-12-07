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
