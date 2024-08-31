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

(pretty-hydra-define
  kdz-pretty-window-resize
  ( :foreign-keys warn
    :title (format "Resize window for: %s (Step size: %d)"
                   (or (buffer-name) "N/A")
                   kdz-window-resize-step--current)
    :quit-key "q"
    :color pink)
  ("Resize Window"
   (("j" kdz/window-dec-height              "Decrease Height")
    ("k" kdz/window-inc-height              "Increase Height")
    ("h" kdz/window-dec-width               "Decrease Width")
    ("l" kdz/window-inc-width               "Increase Width"))
   "Fit Window"
   (("w" kdz/window-fit-to-buffer-width     "Fit to buffer width")
    ("f" kdz/window-fit-to-buffer-fill      "Fit to buffer fill-column")
    ("R" kdz/window-restore-original-width  "Restore to original width"))
   "Select Window"
   (("J" evil-window-down                   "Select window down")
    ("K" evil-window-up                     "Select window down")
    ("H" evil-window-left                   "Select window down")
    ("L" evil-window-right                  "Select window down"))
   "Resize Step"
   (("r" kdz/window-step-size-set-or-reset  "(Re)set Step")
    ("=" kdz/window-step-size-inc           "Increase Step Size")
    ("+" kdz/window-step-size-inc           "Increase Step Size")
    ("_" kdz/window-step-size-dec           "Decrease Step Size")
    ("-" kdz/window-step-size-dec           "Decrease Step Size"))))

(provide 'lib/window-resize)
