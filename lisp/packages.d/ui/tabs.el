(use-package tab-bar
  :config
  (load "lib/tab-bar.el")
  (load "lib/svg-tabs.el")

  ;; TODO Figure out why this isn't affecting the behavior for
  ;;      switching tabs correctly.  Once that's done, I *should*
  ;;      be able to use the partitioned tabs correctly.
  ;; (advice-add tab-bar-tabs-function
  ;;             :filter-return
  ;;             #'kdz/tab-bar-tabs-sort-pinned-tabs-last)

  (setq tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format-svg
        tab-bar-new-tab-to 'rightmost
        tab-bar-format '(tab-bar-separator
                         kdz/tab-bar-format-project-icon
                         tab-bar-separator
                         kdz/tab-bar-format-unpinned-tabs
                         tab-bar-format-align-right
                         tab-bar-separator
                         kdz/tab-bar-format-pinned-tabs
                         tab-bar-separator
                         kdz/tab-bar-format-pin-icon))

  (defun kdz/tab-bar-update-faces (&rest _)
    "Customize tab-bar faces against current theme

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (set-face-attribute 'tab-bar nil
                        :inherit 'mode-line
                        :box (list :line-width 7
                                   :color (face-background 'default))))

  (defun kdz/tab-switch-index-or-select (&optional index)
    "Change tabs, optionally by index using a prefix argument"
    (interactive "P")
    (if (eq index nil)
        (call-interactively 'tab-switch)
      (tab-bar-select-tab index))))

(use-package tab-line
  :config
  (setq tab-line-close-button-show nil
        tab-line-tab-name-truncated-max 40
        tab-line-new-button-show nil)

  (defvar kdz-tab-line-pretty-name-alist
    '(("\\*Embark Collect: consult-ripgrep - #" . "Search: "))
    "Regexp replacements for tab-line tab names")

  (defun kdz/tab-line-tab-name-as-search-results (name)
    (replace-regexp-in-string "\\*Embark Collect: consult-ripgrep - #"
                              "Search: "
                              name))

  (advice-add #'tab-line-tab-name-buffer
              :filter-return
              (lambda (val) (concat " " val " ")))

  (advice-add #'tab-line-tab-name-buffer
              :filter-return #'kdz/tab-line-tab-name-as-search-results)

  (defun kdz/window-left-dwim ()
    (interactive)
    (if (and (eq 'bottom (window-parameter nil 'window-side))
             tab-line-mode)
        (when (not (eq (current-buffer)
                       (car (tab-line-tabs-window-buffers))))
          (tab-line-switch-to-prev-tab))
      (evil-window-left 1)))

  (defun kdz/window-right-dwim ()
    (interactive)
    (if (and (eq 'bottom (window-parameter nil 'window-side))
             tab-line-mode)
        (when (not (eq (current-buffer)
                       (car (last (tab-line-tabs-window-buffers)))))
          (tab-line-switch-to-next-tab))
      (evil-window-right 1)))

  (defun kdz/ensure-bottom-tab-line (&rest args)
    (when (and (eq 'bottom (window-parameter nil 'window-side))
               (not tab-line-mode))
      (tab-line-mode 1)))

  (add-hook 'window-state-change-hook #'kdz/ensure-bottom-tab-line)

  (defun kdz/tab-line-update-faces (&rest _)
    "Customize tab-bar faces against current theme

This is performed via a function so it can be used as a hook on
actions that would update colors in emacs (such as changing themes)"
    (set-face-attribute 'tab-line nil
                        :inherit 'tab-bar
                        :box (list :line-width 7
                                   :color (face-background 'tab-line nil t)
                                   :style 'flat)
                        :underline (list :color 'foreground-color
                                         :style 'line
                                         :position -7))
    (set-face-attribute 'tab-line-tab nil :inherit 'tab-line
                        :box (list :line-width 7
                                   :color (face-background 'tab-line nil t)
                                   :style 'flat))
    (set-face-attribute 'tab-line-tab-current nil
                        :inherit 'tab-line-tab
                        :inverse-video t
                        :box (list :line-width 7
                                   :color (face-foreground 'tab-line-tab nil t)
                                   :style 'flat)
                        :underline (list :color (face-foreground 'tab-line nil t)
                                         :style 'line
                                         :position -7))))

(provide 'packages.d/ui/tabs)
