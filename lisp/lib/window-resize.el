(defvar kdz-window-resize-step-default 5)
(defvar kdz-window-resize-step--current 5)
(defvar kdz-window-resize-fill-column-margin 5)
(defvar kdz-window-resize-fit-buffer-margin 2)

(defun kdz/window-inc-height (count)
  (interactive "p")
  (enlarge-window (or count kdz-window-resize-step--current)))

(defun kdz/window-dec-height (count)
  (interactive "p")
  (enlarge-window (* -1 (or count kdz-window-resize-step--current))))

(defun kdz/window-inc-width (count)
  (interactive "P")
  (message "Resize by %d" (or count kdz-window-resize-step--current))
  (enlarge-window (or count kdz-window-resize-step--current) t))

(defun kdz/window-dec-width (count)
  (interactive "P")
  (message "Resize by %d" (or count kdz-window-resize-step--current))
  (enlarge-window (* -1 (or count kdz-window-resize-step--current)) t))

(defun kdz/window-step-size-inc ()
  (interactive)
  (setq kdz-window-resize-step--current
        (+ kdz-window-resize-step--current 1)))

(defun kdz/window-step-size-dec ()
  (interactive)
  (setq kdz-window-resize-step--current
        (- kdz-window-resize-step--current 1)))

(defun kdz/window-step-size-set-or-reset (count)
  (interactive "p")
  (setq kdz-window-resize-step--current kdz-window-resize-step-default))

(defun kdz/buffer-max-width ()
  (save-excursion
    (let ((max-width 0))
      (beginning-of-buffer)
      (end-of-line)
      (while (not (eobp))
        (setq max-width (max max-width
                             (length (thing-at-point 'line))))
        (next-line)
        (end-of-line))
      max-width)))

(defun kdz/window-fit-to-buffer-width ()
  (interactive)
  (setq-local kdz-original-window-width (window-width))
  (evil-window-set-width (+ (kdz/buffer-max-width)
                            (line-number-display-width)
                            kdz-window-resize-fit-buffer-margin)))

(defun kdz/window-fit-to-buffer-fill ()
  (interactive)
  (setq-local kdz-original-window-width (window-width))
  (evil-window-set-width (+ fill-column
                            (line-number-display-width)
                            kdz-window-resize-fill-column-margin)))

(defun kdz/window-restore-original-width ()
  (interactive)
  (when (boundp 'kdz-original-window-width)
    (evil-window-set-width kdz-original-window-width)
    (makunbound 'kdz-original-window-width)))

(kdz/leader-window-def
  "h" '("Left"                 . kdz/window-left-dwim)
  "l" '("Right"                . kdz/window-right-dwim)
  "f" '("Fit to buffer width"  . kdz/window-fit-to-buffer-width)
  "F" '("Fit to buffer fill"   . kdz/window-fit-to-buffer-fill)
  "r" '("Restore window width" . kdz/window-restore-original-width)
  "s" '("Resize Window"        . kdz-pretty-window-resize/body))

(provide 'lib/window-resize)
