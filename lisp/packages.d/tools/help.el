(use-package helpful :straight t)
(use-package general :straight t)

(use-package which-key
  :straight t
  :demand t
  :init
  (setq which-key-sort-order 'which-key-prefix-then-key-order
	which-key-max-display-columns 5
	which-key-sort-uppercase-first nil
	which-key-prefix-prefix "☰ ")
  (which-key-mode))

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
