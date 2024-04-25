(defvar tap--preview-length 90
  "Max length to show for previewing a thing-at-point")
(defvar tap--preview-truncate "..."
  "Indicator that the preview of a thing is truncated")

(defvar tap--precedence
  '(region uuid url email filename symbol sentence line)
  "Lookup order when trying to guess the thing at a given point")

(defun tap/annotate (type)
  "Provide an annotation of the thing of TYPE at point"
  (tap/preview (tap/thing-of type)))

(defun tap/thing-of (type)
  "Get the raw thing of TYPE at point"
  (let* ((thing (thing-at-point (tap/normalize-type type))))
    (when thing (substring-no-properties thing 0 (length thing)))))

(defun tap/normalize-type (type)
  (cond ((symbolp type) type)
	((stringp type) (intern (downcase type)))
	(t (error "Can't normalize value as symbol" type))))

(defun tap/bounds-of-region-at-point ()
  "Return the start/end bounds for a region, if a region exists."
  (and (use-region-p) (cons (mark) (point))))

(defun tap/add-region-type ()
  "Allow specifying `region' as a type of thing-at-point"
  (put 'region
       'bounds-of-thing-at-point
       'tap/bounds-of-region-at-point))

(defun tap/preview (thing)
  "Give a 1-line preview of THING for consult annotations"
  (when thing
    (let* ((lines (split-string thing "\n"))
	 (first-line (car lines))
	 (rest (seq-filter (lambda (line) (not (string= "" line)))
			   (cdr lines)))
	 (remaining (length (cdr rest)))
	 (line-count (when (> remaining 0)
		       (format " (+%d lines)" remaining)))
	 (truncated-hint (when (or (> (length first-line)
				      tap--preview-length)
				   line-count)
			   tap--preview-truncate))
	 (total-length (+ (length first-line)
			  (length line-count)
			  (length truncated-hint)))
	 (preview-length (min total-length tap--preview-length))
	 (truncate-to (- preview-length
			 (length truncated-hint)
			 (length line-count))))
      (concat ": "
	      (substring first-line 0 truncate-to)
	      truncated-hint
	      line-count))))

(defmacro tap/annotate-for-buffer (buffer-name)
  "Generate thing-at-point annotation with thing from BUFFER-NAME"
  `(lambda (type-string)
    (with-current-buffer ,buffer-name (tap/annotate type-string))))

(defmacro tap/predicate-for-buffer (buffer-name)
  "Check existence of a thing-at-point with thing from BUFFER-NAME"
  `(lambda (type-string)
    (with-current-buffer ,buffer-name (tap/thing-of type-string))))

;; TODO Extract annotation into a marginalia annotator?
(defun tap/select ()
  (let* ((target (buffer-name (current-buffer)))
	 (annotate (tap/annotate-for-buffer target))
	 (predicate (tap/predicate-for-buffer target))
	 (make-candidate (lambda (type)
			   (capitalize (symbol-name type))))
	 (selected-type (completing-read
			 "Thing at point: "
			 (lambda (str pred action)
			   (if (eq action 'metadata)
			       (list 'metadata
				     (cons 'annotation-function annotate)
				     (cons 'category 'thing-at-point))
			     (all-completions str
					      (mapcar make-candidate tap--precedence)
					      pred)))
			 predicate)))
    (when selected-type (cons selected-type (tap/thing-of selected-type)))))

(defun tap/select-dwim ()
  "Return the first thing-at-point that is not nil"
  (let ((selected-type (car (seq-drop-while (lambda (type) (not (tap/thing-of type)))
					    tap--precedence))))
    (when selected-type (cons selected-type (tap/thing-of selected-type)))))

(defun tap/act (action)
  (let ((thing (tap/select)))
    (if thing
	(funcall action (car thing) (cdr thing))
      (message "Nothing to act on (type: %s)!" (car thing)))))

(defun tap/act-dwim (action)
  (let ((thing (cdr (tap/select-dwim))))
    (if thing
	(funcall action thing)
      (message "Nothing to act on!"))))

(defun tap/consult-ripgrep ()
  (interactive)
  (tap/act (lambda (type thing) (consult-ripgrep nil thing))))

(defun tap/consult-ripgrep-dwim ()
  (interactive)
  (tap/act-dwim (lambda (thing) (consult-ripgrep nil thing))))

(provide 'tap)
