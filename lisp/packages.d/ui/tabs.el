(use-package tab-bar
  :after (custom nerd-icons)
  :ensure nil
  :custom
  (tab-bar-auto-width nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'kdz/tab-bar-tab-name-format)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-format '(kdz/tab-bar-format-workspaces
                    tab-bar-format-align-right
                    kdz/tab-bar-format-pinned-tabs))
  :general
  (kdz/leader-tab-def
    "TAB" '("Select Workspace"       . kdz/tab-switch-index-or-select)
    "h"   '("Previous Workspace"     . tab-previous)
    "l"   '("Next Workspace"         . tab-next)
    "H"   '("Move Tab Left"          . kdz/tab-move-left)
    "L"   '("Move Tab Right"         . tab-move)
    "d"   '("Close Workspace"        . tab-bar-close-tab)
    "n"   '("Create Named Workspace" . kdz/create-named-tab))
  :hook ((elpaca-after-init   . kdz/tab-bar-initialize-tab-state))
  :config
  (require 'lib/pinned-tabs)

  (defun kdz/tab-move-left () (interactive) (tab-move -1))

  (defvar kdz--tab-bar-box-width 10
    "Sizing of box to draw around tab-bar faces.

This is used to generate face specs when making theme customizations related to
the tab-bar.")

  (defun kdz/tab-bar-face-spec-base (background underline-color &rest specs)
    "Generate a face-spec for the tab-bar with preferred appearance."
    `((t :box       ,(list :line-width kdz--tab-bar-box-width
                           :color      background
                           :style      'flat-button)
         :underline ,(list :color      underline-color
                           :position   (* -1 kdz--tab-bar-box-width))
         ,@specs)))

  (defvar kdz-blank-buffer-text  "Nothing to see here."
    "Filler text to use in *blank* buffer")

  (defvar kdz-blank-buffer-name  "*blank*" "Placeholder buffer name")

  ;; TODO Change the default directory for new tabs
  (defun kdz/create-named-tab ()
    (interactive)
    (tab-bar-new-tab)
    (tab-bar-rename-tab "*New Tab*")
    (kdz/show-blank-buffer)
    (call-interactively 'tab-bar-rename-tab))

  ;; TODO Make this center the content in the window
  (defun kdz/show-blank-buffer ()
    (interactive)
    (let ((blank-buffer (get-buffer-create kdz-blank-buffer-name)))
      (with-current-buffer blank-buffer
        (unless
            (save-excursion
              (beginning-of-buffer)
              (search-forward kdz-blank-buffer-text nil t))
          (insert kdz-blank-buffer-text))
        (hide-mode-line-mode 1))
      (display-buffer-full-frame blank-buffer '((inhibit-same-window . t)))))

  (defun kdz/tab-bar-initialize-tab-state ()
    (tab-bar-select-tab-by-name "Home")
    (tab-bar-close-tab-by-name "*default*")))

(use-package tab-line
  :after (custom nerd-icons)
  :ensure nil
  :custom
  (tab-line-close-button-show nil)
  (tab-line-tab-name-truncated-max 40)
  (tab-line-new-button-show nil)
  (tab-line-tab-name-function #'kdz/tab-line-buffer-display-name)
  (tab-line-tab-name-format-function #'kdz/tab-line-tab-name-format)
  :hook ((window-state-change . kdz/ensure-bottom-tab-line))
  :config (require 'lib/tab-line-extras))

(provide 'packages.d/ui/tabs)
