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

(defun kdz/coalesce-buffer-windows ()
  "Delete windows for the current buffer that aren't the currently selected window"
  (interactive)
  ;; TODO Ensure selected-window is non-nil and we have a buffer to work with
  ;; TODO Verify this equality check works
  (dolist (window (get-buffer-window-list))
    (when (not (eq window (selected-window)))
      (delete window))))

(defun kdz/switch-to-buffer-current-window ()
  (interactive)
  (let ((switch-to-buffer-obey-display-actions nil))
    (consult-buffer)))

(provide 'lib/misc-actions)
