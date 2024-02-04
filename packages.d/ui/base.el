(use-package hydra
  :straight t
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `(:poshandler posframe-poshandler-frame-center
                      :internal-border-width 2
                      :internal-border-color "#61AFEF"
                      :left-fringe 16
                      :right-fringe 16)))

(use-package sideline
  :straight t
  :init
  (setq sideline-backends-right '((sideline-blame . down)))
  (setq sideline-backends-right-skip-current-line nil))

(use-package nerd-icons :straight t)

(use-package posframe
  :straight t
  :config
  (defvar kdz--posframe-offset-top-percent 10)
  (defvar kdz--posframe-offset-bottom-percent 10)

  (defun kdz/posframe-center-width (info)
    (round
     (* 0.5 (- (plist-get info :parent-frame-width)
               (plist-get info :posframe-width)))))

  (defun kdz/posframe-offset-top (info)
    (let ((offset-percent (/ kdz--posframe-offset-top-percent 100.0))
          (frame-height (plist-get info :parent-frame-height)))
      (cons (kdz/posframe-center-width info)
            (round (* offset-percent frame-height)))))

  (defun kdz/posframe-offset-bottom (info)
    (let* ((parent-frame-height (plist-get info :parent-frame-height))
           (posframe-height (plist-get info :posframe-height))
           (offset-percent (/ kdz--posframe-offset-bottom-percent 100.0)))
      (cons (kdz/posframe-center-width info)
            (round (- parent-frame-height
                      posframe-height
                      (* offset-percent parent-frame-height)))))))

(use-package transient
  :straight t
  :config
  (setq transient-history-file (kdz/local "transient/history.el")
        transient-levels-file (kdz/local "transient/levels.el")
        transient-values-file (kdz/local "transient/values.el")))
