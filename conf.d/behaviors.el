(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
	     '("\\*Messages\\*" display-buffer-in-new-tab nil))
(add-to-list 'display-buffer-alist
	     '("\\*Ibuffer\\*" display-buffer-in-new-tab nil))
(add-to-list 'display-buffer-alist
	     '("\\*Warnings\\*" display-buffer-in-new-tab nil))
(add-to-list 'display-buffer-alist
	     '("\\*ielm\\*" display-buffer-in-side-window '((side . bottom)
							    (slot . 99)
							    (dedicated . t))))
