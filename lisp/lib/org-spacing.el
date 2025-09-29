;;; org-spacing.el --- Add visual spacing between org headings -*- lexical-binding: t; -*-

;;; Commentary:
;; Minor mode that uses overlays to show empty lines between org headings
;; when there is visible content between them or when headings are at different levels.

;;; Code:

(defvar-local kdz/org-spacing--overlays nil
  "List of overlays created by org-spacing mode.")

(defun kdz/org-spacing--clear-overlays ()
  "Remove all org-spacing overlays from the current buffer."
  (dolist (overlay kdz/org-spacing--overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq kdz/org-spacing--overlays nil))

(defun kdz/org-spacing--heading-has-visible-content-p (heading-pos)
  "Check if the heading at HEADING-POS has visible content before the next heading."
  (save-excursion
    (goto-char heading-pos)
    (let ((heading-end (line-end-position))
          (next-heading (save-excursion
                          (outline-next-heading)
                          (point))))
      ;; Check if there's visible non-whitespace content between heading and next heading
      (goto-char heading-end)
      (forward-line 1)
      (catch 'found-content
        (while (< (point) next-heading)
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            ;; Check if this line is visible (not hidden by folding)
            (unless (org-invisible-p line-start)
              ;; Check if line has non-whitespace content
              (unless (string-match-p "\\`[[:space:]]*\\'"
                                      (buffer-substring-no-properties line-start line-end))
                (throw 'found-content t))))
          (forward-line 1))
        nil))))

(defun kdz/org-spacing--content-ends-with-visible-newline-p (heading-pos)
  "Check if the content at HEADING-POS ends with a visible newline."
  (save-excursion
    (goto-char heading-pos)
    (let ((next-heading (save-excursion
                          (outline-next-heading)
                          (point))))
      ;; Go to the line before the next heading
      (goto-char next-heading)
      (forward-line -1)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Check if this line is visible and empty (just whitespace)
        (and (not (org-invisible-p line-start))
             (string-match-p "\\`[[:space:]]*\\'"
                             (buffer-substring-no-properties line-start line-end)))))))

(defun kdz/org-spacing--get-heading-level (pos)
  "Get the level of the org heading at POS."
  (save-excursion
    (goto-char pos)
    (when (org-at-heading-p)
      (org-outline-level))))

(defun kdz/org-spacing--heading-is-folded-p (heading-pos)
  "Check if the heading at HEADING-POS is collapsed/folded."
  (save-excursion
    (goto-char heading-pos)
    (when (org-at-heading-p)
      (org-invisible-p (line-end-position)))))

(defun kdz/org-spacing--find-ancestor-at-level (start-pos target-level)
  "Find the ancestor heading of START-POS that is at TARGET-LEVEL.
Returns the position of the ancestor heading, or nil if not found."
  (save-excursion
    (goto-char start-pos)
    (while (and (org-up-heading-safe)
                (> (org-outline-level) target-level)))
    (when (and (org-at-heading-p)
               (= (org-outline-level) target-level))
      (point))))

(defun kdz/org-spacing--should-add-spacing-p (current-pos next-pos)
  "Determine if spacing should be added between headings at CURRENT-POS and NEXT-POS."
  (let ((current-level (kdz/org-spacing--get-heading-level current-pos))
        (next-level (kdz/org-spacing--get-heading-level next-pos))
        (has-visible-content (kdz/org-spacing--heading-has-visible-content-p current-pos))
        (ends-with-newline (kdz/org-spacing--content-ends-with-visible-newline-p current-pos))
        (current-is-folded (kdz/org-spacing--heading-is-folded-p current-pos)))
    ;; Add spacing if:
    ;; 1. There is visible content between headings, OR
    ;; 2. The headings are at different levels
    ;; BUT NOT if:
    ;; - The content already ends with a visible newline, OR
    ;; - Current heading has no content AND next heading is a sub-heading, OR
    ;; - Current heading is collapsed/folded (UNLESS it's at greater depth than next heading
    ;;   AND its ancestor at the same level as next heading is not also collapsed)
    (and (or has-visible-content
             (not (= current-level next-level)))
         (not ends-with-newline)
         (not (and (not has-visible-content)
                   (> next-level current-level)))
         (not (and current-is-folded
                   (or (<= current-level next-level)
                       (let ((ancestor-pos (kdz/org-spacing--find-ancestor-at-level current-pos next-level)))
                         (and ancestor-pos
                              (kdz/org-spacing--heading-is-folded-p ancestor-pos)))))))))

(defun kdz/org-spacing--add-overlays ()
  "Add spacing overlays between org headings where appropriate."
  (kdz/org-spacing--clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (outline-next-heading)
      (let ((current-pos (point))
            (next-pos (save-excursion
                        (when (outline-next-heading)
                          (point)))))
        (when (and next-pos
                   (kdz/org-spacing--should-add-spacing-p current-pos next-pos))
          ;; Create overlay at the beginning of the next heading
          (let ((overlay (make-overlay next-pos next-pos)))
            (overlay-put overlay 'before-string "\n")
            (overlay-put overlay 'kdz/org-spacing t)
            (push overlay kdz/org-spacing--overlays)))))))

(defun kdz/org-spacing--update-overlays (&rest _)
  "Update spacing overlays after buffer changes."
  (when kdz/org-spacing-mode
    (run-with-idle-timer 0.1 nil #'kdz/org-spacing--add-overlays)))

(defun kdz/org-spacing--update-overlays-on-visibility-change (&rest _)
  "Update spacing overlays after visibility changes (folding/unfolding)."
  (when kdz/org-spacing-mode
    (kdz/org-spacing--add-overlays)))

(define-minor-mode kdz/org-spacing-mode
  "Minor mode to add visual spacing between org headings."
  :lighter " OrgSpacing"
  :keymap nil
  (if kdz/org-spacing-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (user-error "org-spacing-mode can only be enabled in org-mode buffers"))
        (kdz/org-spacing--add-overlays)
        (add-hook 'after-change-functions #'kdz/org-spacing--update-overlays nil t)
        (add-hook 'org-cycle-hook #'kdz/org-spacing--update-overlays-on-visibility-change nil t)
        (add-hook 'org-after-visibility-change-hook #'kdz/org-spacing--update-overlays-on-visibility-change nil t))
    (progn
      (kdz/org-spacing--clear-overlays)
      (remove-hook 'after-change-functions #'kdz/org-spacing--update-overlays t)
      (remove-hook 'org-cycle-hook #'kdz/org-spacing--update-overlays-on-visibility-change t)
      (remove-hook 'org-after-visibility-change-hook #'kdz/org-spacing--update-overlays-on-visibility-change t))))

(provide 'lib/org-spacing)
;;; org-spacing.el ends here
