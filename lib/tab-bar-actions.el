(defun kdz/tab-switch-index-or-select (&optional index)
  (interactive "P")
  (if (eq index nil)
      (call-interactively 'tab-switch)
    (tab-bar-select-tab index)))



