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
