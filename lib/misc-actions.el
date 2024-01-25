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
