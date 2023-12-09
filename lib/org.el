(defvar kdz--org-heading-regexp "^\*+ .*$")

(defun kdz/org-point-is-heading-p ()
  "Check if the current point in a buffer is an org-mode heading"
  (string-match-p kdz--org-heading-regexp (thing-at-point 'line)))

(defun kdz/org-heading-fixup-new-line ()
  "Ensure an empty line between non-empty org-mode headings"
  (when (kdz/org-point-is-heading-p)
    (save-excursion
      (previous-line)
      (when (and (thing-at-point 'line)
		 (not (kdz/org-point-is-heading-p)))
	(goto-char (line-end-position))
	(insert "\n")))))
