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

(use-package which-key-posframe
  :straight t
  :config
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

  (advice-add #'which-key-posframe--show-buffer
              :override
              #'kdz/fixup--which-key-posframe--show-buffer)

  (setq which-key-posframe-poshandler 'kdz/posframe-offset-bottom)
  (which-key-posframe-mode 1))

(use-package vertico-posframe
  :straight t
  :after (posframe vertico)
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler #'kdz/posframe-offset-top)
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8))))

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

(provide 'packages.d/ui/posframe)
