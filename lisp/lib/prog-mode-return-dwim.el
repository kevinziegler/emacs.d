(defvar kdz-comment-indented-keywords
  (append (if (boundp 'hl-todo-keyword-faces)
              (mapcar #'car hl-todo-keyword-faces)
            '())
          '("+" "-" "@param" "@field" "@type" "@return"))
  "Keywords that should be treated as indentation on subsequent lines
when moving to a newline in a comment.")

(defun kdz/comment-components-regexp ()
  "Build a regexp to find the component parts of a comment"
  (rx-to-string
   `(: (group (regexp ,comment-start-skip))
       (group (optional (or ,@kdz-comment-indented-keywords)))
       (group (optional (* blank)))
       (group (optional (* not-newline)))
       line-end)))

(defun kdz/comment-line-components (&optional line)
  "Return a plist of comment parts for LINE.

If a LINE is not supplied, the line of the current point is used."
  (let ((line (or line (thing-at-point 'line))))
    (save-match-data
      (when (string-match (kdz/comment-components-regexp) line)
        (let* ((keyword-match (match-string 2 line))
               (content-match (match-string 4 line))
               (keyword (if (string-empty-p keyword-match)
                            nil
                          keyword-match))
               (content (if (string-empty-p content-match)
                            nil
                          content-match)))
          (list :start (match-string 1 line)
                :keyword keyword
                :extra-whitespace (match-string 3 line)
                :content content
                :empty (and (not keyword) (not content))))))))

(defun kdz/line-comment-start-p ()
  "Test if the current line starts as a comment"
  (and (comment-only-p (line-beginning-position) (line-end-position))
       (string-match-p (rx-to-string `(: bol (regexp ,comment-start)))
                       (thing-at-point 'line))))

(defun kdz/comment-is-next-level-up-p (base-depth candidate)
  "Test if CANDIDATE is a level of indentation up from BASE-DEPTH

To be valid as a next level of indentation, CANDIDATE must have:
- Some amount of whitespace after the initial comment-start marker
- Less leading indentation (including any keyword and whitespace after
  the keyword) as compared to BASE-DEPTH, OR
- The same amount of whitespace as BASE-DEPTH, and a keyword value"
  (let ((depth (+ (length (plist-get candidate :start))
                  (length (plist-get candidate :keyword))
                  (length (plist-get candidate :extra-whitespace)))))
    (and (plist-get candidate :start)
         (string-match-p "[[:blank:]]+$" (plist-get candidate :start))
         (or (< depth base-depth)
             (and (= depth base-depth)
                  (plist-get candidate :keyword))))))

(defun kdz/comment-get-next-indent-option-up (base)
  "Return components for the next level of indentation up from BASE

If a candidate line is found, the components plist for that line is
returned.  If no viable candidate is found, return NIL instead.

NOTE This function assumes the starting point in the buffer is at the
     same line as BASE, and starts searching from there."
  (if (plist-get base :keyword)
      ;; If we are at an empty "keyword" line, the next level of
      ;; indentation up from where we are can be found simply by
      ;; dropping that keyword and any trailing whitespace after the
      ;; keyword
      (list :start (plist-get base :start))
    (let ((base-depth (length (plist-get base :start)))
          (next-prototype))
      (save-excursion
        (beginning-of-line)
        (while (and (not next-prototype)
                    (not (bobp))
                    (kdz/line-comment-start-p))
          (let ((candidate (kdz/comment-line-components)))
            (if (kdz/comment-is-next-level-up-p base-depth candidate)
                (setq next-prototype candidate)
              (progn (previous-line)
                     (beginning-of-line))))))
      next-prototype)))

(defun kdz/comment-newline ()
  "Insert a newline from a comment line"
  (let ((components (kdz/comment-line-components)))
    (if (plist-get components :content)
        (let* ((keyword (plist-get components :keyword))
               (keyword-fill (make-string (length keyword) ? )))
          (newline)
          (insert (plist-get components :start)
                  keyword-fill
                  (plist-get components :extra-whitespace)))
      (let ((next (kdz/comment-get-next-indent-option-up components)))
        (beginning-of-line)
        (if next
            (progn
              (kill-line)
              (insert (plist-get next :start)
                      (or (plist-get next :keyword) "")
                      (or (plist-get next :extra-whitespace) "")))
          (if (and (plist-get components :empty)
                   (save-excursion
                     (previous-line)
                     (plist-get (kdz/comment-line-components) :empty)))
              (kill-line)
            (newline))
          )))))

(defun kdz/prog-mode-return-dwim ()
  (interactive)
  (if (kdz/line-comment-start-p)
      (kdz/comment-newline)
    (newline)))
