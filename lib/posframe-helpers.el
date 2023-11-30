(defvar kdz--posframe-offset-top-percent 10)
(defvar kdz--posframe-offset-bottom-percent 10)

(defun kdz/posframe-offset-top (info)
  (let ((offset-percent (/ kdz--posframe-offset-top-percent 100.0)))
    (cons (kdz/posframe-center-width info)
          (round (* offset-percent (plist-get info :parent-frame-height))))))

(defun kdz/posframe-offset-bottom (info)
  (let* ((parent-frame-height (plist-get info :parent-frame-height))
         (posframe-height (plist-get info :posframe-height))
         (offset-percent (/ kdz--posframe-offset-bottom-percent 100.0)))
    (cons (kdz/posframe-center-width info)
          (round (- parent-frame-height
                    posframe-height
                    (* offset-percent parent-frame-height))))))
