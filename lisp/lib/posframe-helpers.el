(defvar kdz--posframe-offset-top-percent 10)
(defvar kdz--posframe-offset-bottom-percent 10)

(defun kdz/posframe-center-width (info)
  (round
   (* 0.5 (- (plist-get info :parent-frame-width)
             (plist-get info :posframe-width)))))

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

;; Stand-in until the following issue is merged:
;; https://github.com/yanghaoxie/which-key-posframe/pull/21
;;
;; Note that my version *also* tweaks the width parameter
(defun kdz/fixup--which-key-posframe--show-buffer (act-popup-dim)
  "Override which-key-posframe parameters to ensure content is visible"
  (when (posframe-workable-p)
    (save-window-excursion
      (posframe-show
       which-key--buffer
       :font which-key-posframe-font
       :position (point)
       :poshandler which-key-posframe-poshandler
       :background-color (face-attribute 'which-key-posframe :background nil t)
       :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
       :height (ceiling (* 1.25 (car act-popup-dim)))
       :width (ceiling (* 1.1 (cdr act-popup-dim)))
       :internal-border-width which-key-posframe-border-width
       :internal-border-color (face-attribute 'which-key-posframe-border
                                              :background nil
                                              t)
       :override-parameters which-key-posframe-parameters))))

(provide 'lib/posframe-helpers)
